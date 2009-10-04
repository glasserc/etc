"""
    wordpresslib.py

    WordPress xml-rpc client library
    use MovableType API

    Copyright (C) 2005 Michele Ferretti
    black.bird@tiscali.it
    http://www.blackbirdblog.it

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA       02111-1307, USA.

    XML-RPC supported methods:
        * getUsersBlogs
        * getUserInfo
        * getPost
        * getRecentPosts
        * newPost
        * editPost
        * deletePost
        * newMediaObject
        * getCategoryList
        * getPostCategories
        * setPostCategories
        * getTrackbackPings
        * publishPost
        * getPingbacks

    References:
        * http://codex.wordpress.org/XML-RPC_Support
        * http://www.sixapart.com/movabletype/docs/mtmanual_programmatic.html
        * http://docs.python.org/lib/module-xmlrpclib.html
"""

__author__ = "Michele Ferretti <black.bird@tiscali.it>"
__version__ = "$Revision: 1.0 $"
__date__ = "$Date: 2005/05/02 $"
__copyright__ = "Copyright (c) 2005 Michele Ferretti"
__license__ = "LGPL"

import exceptions
import re
import os
import xmlrpclib
import datetime
import time
from functools import wraps
import mimetypes
import warnings

class WordPressException(exceptions.Exception):
    """Custom exception for WordPress client operations
    """
    def __init__(self, obj):
        if isinstance(obj, xmlrpclib.Fault):
            self.id = obj.faultCode
            self.message = obj.faultString
        else:
            self.id = 0
            self.message = obj

    def __str__(self):
        return '<%s %d: \'%s\'>' % (self.__class__.__name__, self.id, self.message)

class WordPressBlog():
    """Represents blog item
    """
    def __init__(self, id=None, name=None, url=None, isAdmin=None):
        self.id = id or ''
        self.name = name or ''
        self.url = url or ''
        self.isAdmin = isAdmin or False

    @classmethod
    def from_xmlrpc(cls, blog):
        return cls(
            id      = blog['blogid'],
            name    = blog['blogName'],
            isAdmin = blog['isAdmin'],
            url     = blog['url'],
            )


class WordPressUser():
    """Represents user item
    """
    def __init__(self, id=None, firstname=None, lastname=None, nickname=None,
                 email=None):
        self.id = id or ''
        self.firstname = firstname or ''
        self.lastname = lastname or ''
        self.nickname = nickname or ''
        self.email = email or ''

    def get_firstName(self):
        warnings.warn('firstName is deprecated; use firstname',
                      DeprecationWarning)
        return self.firstname
    firstName = property(get_firstName)

    def get_lastName(self):
        warnings.warn('lastName is deprecated; use lastname',
                      DeprecationWarning)
        return self.lastname
    lastName = property(get_lastName)

    @classmethod
    def from_xmlrpc(cls, userinfo):
        return cls(
            id        = userinfo['userid'],
            firstName = userinfo['firstname'],
            lastName  = userinfo['lastname'],
            nickname  = userinfo['nickname'],
            email     = userinfo['email'],
            )


class WordPressCategory():
    """Represents category item
    """
    def __init__(self, id=None, name=None, isPrimary=None):
        self.id = id or 0
        self.name = name or ''
        self.isPrimary = isPrimary or False

    @classmethod
    def from_xmlrpc(cls, cat):
        return cls(id        = int(cat['categoryId']),
                   name      = cat['categoryName'],
                   isPrimary = cat.get('isPrimary', False))


class WordPressPost():
    """Represents post item
    """
    def __init__(self, id=None, title=None, date=None, permaLink=None,
                 description=None, textMore=None, excerpt=None, link=None,
                 categories=None, user=None, allowPings=None,
                 allowComments=None):
        self.id = id or 0
        self.title = title or ''
        self.date = date or None
        self.permaLink = permaLink or ''
        self.description = description or ''
        self.textMore = textMore or ''
        self.excerpt = excerpt or ''
        self.link = link or ''
        self.categories = categories or []
        self.user = user or ''
        self.allowPings = allowPings or False
        self.allowComments = allowComments or False


def wordpress_call(func):
    '''Decorator that handles the try/catch XMLRPC wrapping'''
    @wraps(func)
    def call(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except xmlrpclib.Fault, fault:
            raise WordPressException(fault)

    return call

class WordPressClient():
    """Client for connect to WordPress XML-RPC interface
    """

    def __init__(self, url, user, password):
        self.url = url
        self.user = user
        self.password = password
        self.blogId = 0
        self.categories = None
        self._server = xmlrpclib.ServerProxy(self.url)

    def _filterPost(self, post):
        """Transform post struct in WordPressPost instance
        """
        postObj = WordPressPost()
        postObj.permaLink       = post['permaLink']
        postObj.description     = post['description']
        postObj.title           = post['title']
        postObj.excerpt         = post['mt_excerpt']
        postObj.user            = post['userid']
        postObj.date            = time.strptime(str(post['dateCreated']), "%Y%m%dT%H:%M:%S")
        postObj.link            = post['link']
        postObj.textMore        = post['mt_text_more']
        postObj.allowComments   = post['mt_allow_comments'] == 1
        postObj.id              = int(post['postid'])
        postObj.categories      = post['categories']
        postObj.allowPings      = post['mt_allow_pings'] == 1
        return postObj

    def _filterCategory(self, cat):
        """Transform category struct in WordPressCategory instance
        """
        return WordPressCategory.from_xmlrpc(cat)

    def selectBlog(self, blogId):
        # FIXME: this doesn't seem very pythonic
        self.blogId = blogId

    def supportedMethods(self):
        """Get supported methods list
        """
        # FIXME: shouldn't this be self._server.system.listMethods?
        return self._server.mt.supportedMethods()

    supported_methods = supportedMethods

    @wordpress_call
    def get_options(self):
        return self._server.wp.getOptions(self.blogId, self.user, self.password)

    getOptions = get_options

    def getLastPost(self):
        """Get last post
        """
        return tuple(self.getRecentPosts(1))[0]

    get_last_post = getLastPost

    @wordpress_call
    def getRecentPosts(self, numPosts=5):
        """Get recent posts
        """
        posts = self._server.metaWeblog.getRecentPosts(self.blogId, self.user,
                                                       self.password, numPosts)
        for post in posts:
            yield self._filterPost(post)

    get_recent_posts = getRecentPosts

    @wordpress_call
    def getPost(self, postId):
        """Get post item
        """
        return self._filterPost(self._server.metaWeblog.getPost(str(postId), self.user, self.password))

    get_post = getPost

    @wordpress_call
    def getUserInfo(self):
        """Get user info
        """
        userinfo = self._server.blogger.getUserInfo('', self.user, self.password)
        return WordPressUser.from_xmlrpc(userinfo)

    get_user_info = getUserInfo

    @wordpress_call
    def getUsersBlogs(self):
        """Get blog's users info
        """
        blogs = self._server.blogger.getUsersBlogs('', self.user, self.password)
        for blog in blogs:
            blogObj = WordPressBlog.from_xmlrpc(blog)
            yield blogObj

    get_users_blogs = getUsersBlogs

    def newPost(self, post, publish):
        """Insert new post
        """
        blogContent = {
            'title' : post.title,
            'description' : post.description
        }

        # add categories
        i = 0
        categories = []
        for cat in post.categories:
            if i == 0:
                categories.append({'categoryId' : cat, 'isPrimary' : 1})
            else:
                categories.append({'categoryId' : cat, 'isPrimary' : 0})
            i += 1

        # insert new post
        idNewPost = int(self._server.metaWeblog.newPost(self.blogId, self.user, self.password, blogContent, 0))

        # set categories for new post
        self.setPostCategories(idNewPost, categories)

        # publish post if publish set at True
        if publish:
            self.publishPost(idNewPost)

        return idNewPost

    new_post = newPost

    @wordpress_call
    def getPostCategories(self, postId):
        """Get post's categories
        """
        categories = self._server.mt.getPostCategories(postId, self.user,
                                                self.password)
        for cat in categories:
            yield self._filterCategory(cat)

    get_post_categories = getPostCategories

    @wordpress_call
    def setPostCategories(self, postId, categories):
        """Set post's categories
        """
        self._server.mt.setPostCategories(postId, self.user, self.password, categories)

    set_post_categories = setPostCategories

    def editPost(self, postId, post, publish):
        """Edit post
        """
        blogcontent = {
            'title' : post.title,
            'description' : post.description,
            'permaLink' : post.permaLink,
            'mt_allow_pings' : post.allowPings,
            'mt_text_more' : post.textMore,
            'mt_excerpt' : post.excerpt
        }

        if post.date:
            blogcontent['dateCreated'] = xmlrpclib.DateTime(post.date)

        # add categories
        i = 0
        categories = []
        for cat in post.categories:
            if i == 0:
                categories.append({'categoryId' : cat, 'isPrimary' : 1})
            else:
                categories.append({'categoryId' : cat, 'isPrimary' : 0})
            i += 1

        result = self._server.metaWeblog.editPost(postId, self.user, self.password,
                                              blogcontent, 0)

        if result == 0:
            raise WordPressException('Post edit failed')

        # set categories for new post
        self.setPostCategories(postId, categories)

        # publish new post
        if publish:
            self.publishPost(postId)

    edit_post = editPost

    @wordpress_call
    def deletePost(self, postId):
        """Delete post
        """
        return self._server.blogger.deletePost('', postId, self.user,
                                         self.password)

    delete_post = deletePost

    @wordpress_call
    def getCategoryList(self):
        """Get blog's categories list
        """
        if not self.categories:
            self.categories = []
            categories = self._server.mt.getCategoryList(self.blogId,
                                            self.user, self.password)
            for cat in categories:
                self.categories.append(self._filterCategory(cat))

        return self.categories

    get_category_list = getCategoryList

    def getCategoryIdFromName(self, name):
        """Get category id from category name
        """
        for c in self.getCategoryList():
            if c.name == name:
                return c.id

    get_category_id_from_name = getCategoryIdFromName

    @wordpress_call
    def getTrackbackPings(self, postId):
        """Get trackback pings of post
        """
        return self._server.mt.getTrackbackPings(postId)

    get_trackback_pings = getTrackbackPings

    @wordpress_call
    def publishPost(self, postId):
        """Publish post
        """
        return (self._server.mt.publishPost(postId, self.user, self.password) == 1)

    publish_post = publishPost

    @wordpress_call
    def getPingbacks(self, postUrl):
        """Get pingbacks of post
        """
        return self._server.pingback.extensions.getPingbacks(postUrl)

    get_pingbacks = getPingbacks

    @wordpress_call
    def newMediaObject(self, mediaFileName):
        """Add new media object (image, movie, etc...)
        """
        warnings.warn('newMediaObject is deprecated; use uploadFile instead',
                      DeprecationWarning)

        f = file(mediaFileName, 'rb')
        mediaBits = f.read()
        f.close()

        mediaStruct = {
            'name' : os.path.basename(mediaFileName),
            'bits' : xmlrpclib.Binary(mediaBits)
        }

        result = self._server.metaWeblog.newMediaObject(self.blogId,
                                self.user, self.password, mediaStruct)
        return result['url']

    def upload_file(self, filename, overwrite=False):
        f = file(filename, 'rb')
        bits = f.read()
        f.close()

        media_structZ = {
            'name': os.path.basename(filename),
            'bits': xmlrpclib.Binary(bits),
            'type': mimetypes.guess_type(filename),
            'overwrite': overwrite,
            }

        result = self._server.wp.uploadFile(self.blogId,
                                self.user, self.password, media_struct)
        return result['url']
