#! /usr/bin/env python
# Copyright 2013, David Bremner <david@tethera.net>

# Licensed under the same terms as notmuch.

import notmuch
import re
import os, errno
import sys
from collections import defaultdict
import argparse
import hashlib

# skip automatic and maildir tags

skiptags = re.compile(r"^(attachement|signed|encrypted|draft|flagged|passed|replied|unread)$")

# some random person on stack overflow suggests:

def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else: raise

VERBOSE = False

def log(msg):
    if VERBOSE:
        print(msg)

CHARSET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+_@=.,-'

encode_re = '([^{0}])'.format(CHARSET)

decode_re = '[%]([0-7][0-9A-Fa-f])'

def encode_one_char(match):
    return('%{:02x}'.format(ord(match.group(1))))

def encode_for_fs(str):
    return re.sub(encode_re,encode_one_char, str,0)

def mangle_message_id(msg_id):
    """
    Return a mangled version of the message id, suitable for use as a filename.
    """
    MAX_LENGTH = 143
    FLAGS_LENGTH = 8    # :2,S...??
    encoded = encode_for_fs(msg_id)
    if len(encoded) < MAX_LENGTH - FLAGS_LENGTH:
        return encoded

    SHA_LENGTH = 8
    TRUNCATED_ID_LENGTH = MAX_LENGTH - SHA_LENGTH - FLAGS_LENGTH
    PREFIX_LENGTH = SUFFIX_LENGTH = (TRUNCATED_ID_LENGTH - 3) // 2
    prefix = encoded[:PREFIX_LENGTH]
    suffix = encoded[-SUFFIX_LENGTH:]
    sha = hashlib.sha256()
    sha.update(encoded)
    return prefix + '...' + suffix + sha.hexdigest()[:SHA_LENGTH]

def decode_one_char(match):
    return chr(int(match.group(1),16))

def decode_from_fs(str):
    return re.sub(decode_re,decode_one_char, str, 0)

def mk_tag_dir(tagdir):

    mkdir_p (os.path.join(tagdir, 'cur'))
    mkdir_p (os.path.join(tagdir, 'new'))
    mkdir_p (os.path.join(tagdir, 'tmp'))


flagpart = '(:2,[^:]*)'
flagre = re.compile(flagpart + '$');

def path_for_msg (dir, msg):
    filename = msg.get_filename()
    flagsmatch = flagre.search(filename)
    if flagsmatch == None:
        flags = ''
    else:
        flags = flagsmatch.group(1)

    return os.path.join(dir, 'cur', mangle_message_id(msg.get_message_id()) + flags)


def unlink_message(dir, msg):

    dir = os.path.join(dir, 'cur')

    mangled_id = mangle_message_id(msg.get_message_id())
    filepattern = '^' + re.escape(mangled_id)  + flagpart +'?$'

    filere = re.compile(filepattern)

    for file in os.listdir(dir):
        if filere.match(file):
            log("Unlinking {}".format(os.path.join(dir, file)))
            if not opts.dry_run:
                os.unlink(os.path.join(dir, file))

def dir_for_tag(tag):
    enc_tag = encode_for_fs (tag)
    return os.path.join(tagroot, enc_tag)

disk_tags = defaultdict(set)
disk_ids = set()

def read_tags_from_disk(rootdir):

    for root, subFolders, files in os.walk(rootdir):
        for filename in files:
            mangled_id = filename.split(':')[0]
            tag = root.split('/')[-2]
            disk_ids.add(mangled_id)
            disk_tags[mangled_id].add(decode_from_fs(tag))

# Main program

parser = argparse.ArgumentParser(description='Sync notmuch tag database to/from link farm')
parser.add_argument('-l','--link-style',choices=['hard','symbolic', 'adaptive'],
                    default='adaptive')
parser.add_argument('-d','--destination',choices=['disk','notmuch'], default='disk')
parser.add_argument('-t','--threshold', default=50000L, type=int)
parser.add_argument('-n','--dry-run', default=False, action='store_true')
parser.add_argument('-v','--verbose', default=False, action='store_true')

parser.add_argument('tagroot')

opts=parser.parse_args()
VERBOSE = opts.verbose

tagroot=opts.tagroot

sync_from_links = (opts.destination == 'notmuch')

read_tags_from_disk(tagroot)

if sync_from_links:
    db = notmuch.Database(mode=notmuch.Database.MODE.READ_WRITE)
else:
    db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)

dbtags = filter (lambda tag: not skiptags.match(tag), db.get_all_tags())

if sync_from_links:
    # have to iterate over even untagged messages
    querystr = '*'
else:
    querystr = ' OR '.join(map (lambda tag: 'tag:'+tag,  dbtags))

q_new = notmuch.Query(db, querystr)
q_new.set_sort(notmuch.Query.SORT.UNSORTED)
for msg in q_new.search_messages():

    # silently ignore empty tags
    db_tags = set(filter (lambda tag: tag != '' and not skiptags.match(tag),
                          msg.get_tags()))

    message_id = msg.get_message_id()

    mangled_id = mangle_message_id(message_id)

    disk_ids.discard(mangled_id)

    missing_on_disk = db_tags.difference(disk_tags[mangled_id])
    missing_in_db = disk_tags[mangled_id].difference(db_tags)

    if sync_from_links:
        msg.freeze()

    filename = msg.get_filename()

    if len(missing_on_disk) > 0:
        if opts.link_style == 'adaptive':
            statinfo = os.stat (filename)
            symlink = (statinfo.st_size > opts.threshold)
        else:
            symlink = opts.link_style == 'symbolic'

    for tag in missing_on_disk:

        if sync_from_links:
            log("Removing tag {} from {}".format(tag, message_id))
            if not opts.dry_run:
                msg.remove_tag(tag,sync_maildir_flags=False)
        else:
            tagdir = dir_for_tag (tag)

            if not opts.dry_run:
                mk_tag_dir (tagdir)

            newlink = path_for_msg (tagdir, msg)

            log("Linking {} to {}".format(filename, newlink))
            if not opts.dry_run:
                if symlink:
                    os.symlink(filename, newlink)
                else:
                    os.link(filename, newlink)


    for tag in missing_in_db:
        if sync_from_links:
            log("Adding {} to message {}".format(tag, message_id))
            if not opts.dry_run:
                msg.add_tag(tag,sync_maildir_flags=False)
        else:
            tagdir = dir_for_tag (tag)
            unlink_message(tagdir,msg)

    if sync_from_links:
        msg.thaw()

# everything remaining in disk_ids is a deleted message
# unless we are syncing back to the database, in which case
# it just might not currently have any non maildir tags.

if not sync_from_links:
    for root, subFolders, files in os.walk(tagroot):
        for filename in files:
            mangled_id = filename.split(':')[0]
            if mangled_id in disk_ids:
                os.unlink(os.path.join(root, filename))


db.close()

# currently empty directories are not pruned.
