"""
unpack-storybundle.py: move dir/Title - Author.* into Author/Title/*

Usage:

cd ~/root; unpack-storybundle.py /path/to/storybundle/directory
"""

import os
import re
import sys
import argparse

FILENAME_RE = re.compile(r"(?P<title>.*) - (?P<author>.*)\..{3,5}")

parser = argparse.ArgumentParser(description="organize StoryBundle files")
parser.add_argument("raw_directory", help="path to unpacked StoryBundle files")
parser.add_argument("--dry-run", "-n", action='store_true', help="don't act, only explain what will be done")
parser.add_argument("--verbose", "-v", action='store_true', help="explain a little bit more")

def makedirs(options, target_dir, already_created=None):
    if already_created != None and target_dir in already_created:
        return

    if os.path.exists(target_dir):
        return

    if options.dry_run or options.verbose:
        print("Creating directory {}".format(target_dir))

    if not options.dry_run:
        os.makedirs(target_dir)

    already_created[target_dir] = True

def move_file(options, src_file, target_dir):
    basename = os.path.basename(src_file)
    if options.dry_run or options.verbose:
        print("Moving {} to {}".format(src_file, target_dir))

    if not options.dry_run:
        os.rename(src_file, os.path.join(target_dir, basename))


def main():
    args = parser.parse_args()
    already_created = {}

    for root, dirs, files in os.walk(args.raw_directory):
        print("Looking at files in {}".format(root))
        for filename in files:
            match = FILENAME_RE.match(filename)
            if not match:
                print("Didn't match regex: {}".format(filename))
                continue

            author = match.group("author")
            title = match.group("title")
            if args.verbose:
                print("Book has author {} and title {}".format(author, title))
            target_dir = os.path.join(author, title)
            makedirs(args, target_dir, already_created)

            src_file = os.path.join(root, filename)
            move_file(args, src_file, target_dir)

    if not os.listdir(args.raw_directory):  # now empty
        os.rmdir(args.raw_directory)

if __name__ == '__main__':
    main()
