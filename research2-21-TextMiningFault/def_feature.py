# -*- coding: utf-8 -*-
"""
Created on Wed Jun  9 19:27:26 2021

@author: xinx_
"""

from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
import pandas as pd
import numpy as np
import os
import lda
import jieba

################ function

# 把simpleWordCut的结果转化成sklearn能用的list的fuction
def SWC2Sklearn(resultlist):
    result = []
    for i in range(0, len(resultlist)):
        one = [word for word in resultlist[i]]
        result.append(' '.join(one))
    return result


# 生成TD-IDF词向量function
def ChineseFeatureVec(resultlist, n_topics=5, n_iter=100, random_state=0):

    counter = CountVectorizer()
    counts = counter.fit_transform(resultlist)
    
    # tf-idf
    tfidfer = TfidfTransformer()
    tfidf = tfidfer.fit_transform(counts)
    wordVocabulary = counter.vocabulary_
    wordVector = counts.toarray()
    tfidfVector = tfidf.toarray()
    
    # LDA
    analyze = counter.build_analyzer()
    model = lda.LDA(n_topics=n_topics, n_iter=n_iter, random_state=random_state)
    model.fit(np.asarray(wordVector))
    topic_word = model.topic_word_
    doc_topic = model.doc_topic_    
    
    # result
    result = {'tfidf_wordVoc':wordVocabulary,
              'tfidf_wordVec':wordVector,
              'tfidf_tfidfVec':tfidfVector,
              'lda_doctopicVec':doc_topic}
    return result




################ test

# 导入数据
data_tf = pd.read_csv('原始数据.csv',
                      names=['深层现象', '故障现象', '故障分析', '处理方式'],
                      encoding='utf-8')
data_tf.columns = ['deepph', 'ph', 'analyze', 'handle']
data_tf = data_tf[1:]
data_tf.index = pd.Series(range(1, 880))

stopwords = stopwordslist(r"C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/stopwords/bd_hit_scu_stopwords.txt")
# 对故障现象(ph)进行分词
data_cut_ph = data_tf.ph
list_result_cut_ph = simpleWordCut(pd.Series.tolist(data_cut_ph.astype(str)), stopwords, cut_all=False, HMM=False)
# 提取故障现象(ph)的特征向量
list_4sklearn_ph = SWC2Sklearn(list_result_cut_ph)
dic_result_CFV_ph = ChineseFeatureVec(list_4sklearn_ph, n_topics=5, n_iter=100, random_state=0)
list_result_tfidf_ph = dic_result_CFV_ph['tfidf_tfidfVec']
list_result_lda_ph = dic_result_CFV_ph['lda_doctopicVec']
