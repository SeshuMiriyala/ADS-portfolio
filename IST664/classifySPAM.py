'''
  This program shell reads email data for the spam classification problem.
  The input to the program is the path to the Email directory "corpus" and a limit number.
  The program reads the first limit number of ham emails and the first limit number of spam.
  It creates an "emaildocs" variable with a list of emails consisting of a pair
    with the list of tokenized words from the email and the label either spam or ham.
  It prints a few example emails.
  Your task is to generate features sets and train and test a classifier.

  Usage:  python classifySPAM.py  <corpus directory path> <limit number>
'''
# open python and nltk packages needed for processing
import os
import sys
import random
import nltk
nltk.download('stopwords')
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
import re
import numpy as np
from nltk.corpus import stopwords
from sklearn.model_selection import KFold
from collections import Counter
from nltk.classify.scikitlearn import SklearnClassifier
from nltk.classify.scikitlearn import SklearnClassifier
from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.tree import DecisionTreeClassifier

# define a feature definition function here
def spam_features(emailTokens, word_features):
  specialChars = '[@_!#$%^&*()<>?/\|}{~:\'\',\.-`]'
  string_check= re.compile(specialChars)
  specialCharCount = 0
  nonSpecialCharCount = 0
  numbersCount = 0
  features = {}
  wordsLongerThan6Chars = 0
  numberOfShortWords = 0
  avgWordLength = 0

  for sChar in specialChars:
    features['V_{}'.format(sChar)] = 0

  for token in emailTokens:
    for sChar in specialChars:
      if sChar in token:
          features['V_{}'.format(sChar)] = features['V_{}'.format(sChar)] + 1
    if(string_check.search(token) == None):
      nonSpecialCharCount += 1
    else: 
      specialCharCount += 1
    if token.isnumeric():
      numbersCount += 1
    if len(token) > 6:
      wordsLongerThan6Chars += 1
    if len(token) < 3:
      numberOfShortWords += 1
    avgWordLength += len(token)

  features["SpecialCharCount"] = specialCharCount
  features["NonSpecialCharCount"] = nonSpecialCharCount
  features["numbers"] = numbersCount
  features["wordsLongerThan6Chars"] = wordsLongerThan6Chars
  features["numberOfShortWords"] = numberOfShortWords
  features["avgWordLength"] = avgWordLength/len(emailTokens)

  counts = Counter([token.lower() for token in emailTokens])
  for word in word_features:
    features['V_{}'.format(word)] = counts[word]

  tagged_words = nltk.pos_tag(emailTokens)
  numNoun = 0
  numVerb = 0
  numAdj = 0
  numAdverb = 0
  for (word, tag) in tagged_words:
      if tag.startswith('N'): numNoun += 1
      if tag.startswith('V'): numVerb += 1
      if tag.startswith('J'): numAdj += 1
      if tag.startswith('R'): numAdverb += 1
  #features['nouns'] = numNoun
  #features['verbs'] = numVerb
  #features['adjectives'] = numAdj
  #features['adverbs'] = numAdverb
          
  return features

# Function to compute precision, recall and F-Measure for each label
#  and for any number of labels
# Input: list of gold labels, list of predicted labels (in same order)
# Output:  prints precision, recall and F-Measure for each label
def eval_measures(gold, predicted):
    # get a list of labels
    labels = list(set(gold))
    # these lists have values for each label 
    recall_list = []
    precision_list = []
    F1_list = []
    for lab in labels:
        # for each label, compare gold and predicted lists and compute values
        TP = FP = FN = TN = 0
        for i, val in enumerate(gold):
            if val == lab and predicted[i] == lab:  TP += 1
            if val == lab and predicted[i] != lab:  FN += 1
            if val != lab and predicted[i] == lab:  FP += 1
            if val != lab and predicted[i] != lab:  TN += 1
        # use these to compute recall, precision, F1
        recall = TP / (TP + FP)
        precision = TP / (TP + FN)
        recall_list.append(recall)
        precision_list.append(precision)
        F1_list.append( 2 * (recall * precision) / (recall + precision))

    # the evaluation measures in a table with one row per label
    print('\tPrecision\tRecall\t\tF1')
    # print measures for each label
    for i, lab in enumerate(labels):
        print(lab, '\t', "{:10.3f}".format(precision_list[i]), \
          "{:10.3f}".format(recall_list[i]), "{:10.3f}".format(F1_list[i]))

# Function to evaluate diffirent models from Scikit learn
# Input: List of all featureset created
# Output: prints accuracy for each model
def eval_models(featuresets):
  kf = KFold(n_splits=5)

  models = [
      ('LogisticRegression', LogisticRegression()),
      ('MultinomialNB', MultinomialNB()),
      ('BernoulliNB', BernoulliNB()),
      ('SVM', SVC()),
      ('KNeighborsClassifier', KNeighborsClassifier()),
      ('DecisionTreeClassifierT', DecisionTreeClassifier()),
  ]

  for name, model in models:
    sum = 0
    clf = model
    for train,test in kf.split(featuresets):
      train_data = np.array(featuresets)[train]
      test_data = np.array(featuresets)[test]
      classifier = SklearnClassifier(clf)
      classifier.train(train_data)
      sum += nltk.classify.accuracy(classifier, test_data)
    print (name, 'Accuracy: %.3f' % (sum/5))

# function to read spam and ham files, train and test a classifier 
def processspamham(dirPath,limitStr):
  # convert the limit argument from a string to an int
  limit = int(limitStr)
  
  # start lists for spam and ham email texts
  hamtexts = []
  spamtexts = []
  os.chdir(dirPath)
  # process all files in directory that end in .txt up to the limit
  # assuming that the emails are sufficiently randomized
  for file in os.listdir("./spam"):
    if (file.endswith(".txt")) and (len(spamtexts) < limit):
      # open file for reading and read entire file into a string
      f = open("./spam/"+file, 'r', encoding="latin-1")
      spamtexts.append (f.read())
      f.close()
  for file in os.listdir("./ham"):
    if (file.endswith(".txt")) and (len(hamtexts) < limit):
      # open file for reading and read entire file into a string
      f = open("./ham/"+file, 'r', encoding="latin-1")
      hamtexts.append (f.read())
      f.close()
  
  # print number emails read
  print ("Number of spam files:",len(spamtexts))
  print ("Number of ham files:",len(hamtexts))
  print
  
  # create list of mixed spam and ham email documents as (list of words, label)
  emaildocs = []
  all_spam_words_list = []
  all_ham_words_list = []
  stopwordsList = nltk.corpus.stopwords.words('english') + ['Subject',':']

  # add all the spam
  for spam in spamtexts:
    tokens = nltk.word_tokenize(spam.lower())
    emaildocs.append((tokens, 'spam'))
  # add all the regular emails
  for ham in hamtexts:
    tokens = nltk.word_tokenize(ham.lower())
    emaildocs.append((tokens, 'ham'))
  
  # randomize the list
  random.shuffle(emaildocs)
  
  # print a few token lists
  for email in emaildocs[:4]:
    print (email)
    print('\n')
  
  # possibly filter tokens
  all_ham_words_list = [w.lower() for (word,c) in emaildocs for w in word if c == 'ham' if w not in stopwordsList]
  all_spam_words_list = [w.lower() for (word,c) in emaildocs for w in word if c == 'spam' if w not in stopwordsList]

  # continue as usual to get all words and create word features
  all_spam_words = nltk.FreqDist(all_spam_words_list)
  all_ham_words = nltk.FreqDist(all_ham_words_list)

  spam_word_items = all_spam_words.most_common(2000)
  spam_word_features = [word for (word,count) in spam_word_items]

  ham_word_items = all_ham_words.most_common(2000)
  ham_word_features = [word for (word,count) in ham_word_items]

  spam_exclusive_items = [value for value in spam_word_features if value not in ham_word_features]
  ham_exclusive_items = [value for value in ham_word_features if value not in spam_word_features]
  
  # feature sets from a feature definition function

  # Now we apply the function to the document dataset
  featuresets = [(spam_features(d, spam_exclusive_items + ham_exclusive_items), c) for (d,c) in emaildocs]

  # train classifier and show performance in cross-validation
  #eval_models(featuresets)
  kf = KFold(n_splits=5)
  sum = 0

  for train,test in kf.split(featuresets):
    train_data = np.array(featuresets)[train]
    test_data = np.array(featuresets)[test]
    #classifier = nltk.NaiveBayesClassifier.train(train_data)
    classifier = SklearnClassifier(BernoulliNB())
    classifier.train(train_data)
    sum += nltk.classify.accuracy(classifier, test_data)
 
  # Storing the score in a variable
  accuracy = sum/5

  # find mean accuracy over all rounds
  print ('Accuracy: %.3f' % accuracy)

  goldlist = []
  predictedlist = []
  for (features, label) in test_data:
    goldlist.append(label)
    predictedlist.append(classifier.classify(features))

  eval_measures(goldlist, predictedlist)



"""
commandline interface takes a directory name with ham and spam subdirectories
   and a limit to the number of emails read each of ham and spam
It then processes the files and trains a spam detection classifier.

"""
if __name__ == '__main__':
  if (len(sys.argv) != 3):
    print ('usage: python classifySPAM.py <corpus-dir> <limit>')
    sys.exit(0)
  processspamham(sys.argv[1], sys.argv[2])
        
