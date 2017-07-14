import pandas as pd
import numpy as np 
import re
import datetime
from itertools import tee, islice, chain, izip
# This is a test

############## Import Raw datafile ##############
#cd "/Users/coreyjackson/Desktop/Research Projects/Accuracy in Crowd/AccuracyGS/AccuracyGS"
accuracy = pd.read_csv('gs-accuracy-2-16.csv') # CHANGE NAME OF .CSV FILE
accuracy_lines = accuracy.readline()
#cd "/Users/coreyjackson/Dropbox/INSPIRE/Data/System dumps"
geordi = pd.read_csv('geordi-1-5-17.csv')
 
#cd "/Users/coreyjackson/Dropbox/INSPIRE/Data/Message Experiment"
names = pd.read_csv('GS_id_login.csv', dtype={'id':'str'})


geordi['datetime'] = pd.to_datetime(geordi['datetime'])
accuracy['datetime'] = pd.to_datetime(accuracy['datetime'])

# join names and geordi file 
geordi =  pd.merge(left=geordi, right=names, left_on="userID", right_on="id")
geordi_lines = geordi.readline()

remove names _file 
del names

# Resources http://stackoverflow.com/questions/29813135/python-how-to-create-a-complete-distance-matrix-from-a-row-of-values


##### Variable names in Geordi file 
		# aboutMenuReference =  about-menu
		# addCommentTotal = add-comment
		# breadcrumbTotal = breadcrumb
		# changePageTotal = change-page
		# changeProjectsidebar = change-project-sidebar
		# classificationStartCount = classificationStart
		# closeFieldGuideCount = close-field-guide
		# collectMenuCount = collect-menu
		# deletePostCount = delete-post
		# discussionTime = discussion-time
		# editPostCount = edit-post
		# favouriteTotal = favourite
		# footerMenuCount = footer-menu
		# globeMenuCount = globe-menu
		# hashtagSideBarReference = hashtag-sidebar
		# likePostCount = like-post
		# linkPostCount = link-post
		# loginCount = login
		# logoutCount = logout
		# messageUserCount = message-user
		# metadataReference = metadata
		# newDiscussionCount = new-discussion
		# projectMenuReference = project-menu
		# maxFieldGuideReferences = open-field-guide
		# profileMenuRefernces = profile-menu
		# recentCommentsSidebar = recent-comments-sidebar
		# registerCount = register
		# registerLinkReferences = register-link
		# replyPostCount = reply-post
		# reportPostCount = report-post
		# TimesSearchMade = search
		# searchBackCount = search-back
		# sendMessageCount = send-message
		# subjectImageCount = subject-image
		# NumOfUsersSubscribed = subscribe
		# talkPageReferences = talk-view
		# topMenuReferences = top-menu
		# unfavouriteCount = unfavourite
		# updateCommentCount = update-count
		# viewDiscussionCount = view-discussion
		# viewProfileAuthor = view-profile-author
		# viewProfileSidebar = view-profile-sidebar
		# viewSubjectDirectCount = view-subject-direct

### Loop to start counting cases of variables above



for line1 in accuracy_lines
aboutMenuReference = []

