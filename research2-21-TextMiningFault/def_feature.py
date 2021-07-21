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
import gensim

################ function

# 把simpleWordCut的结果转化成sklearn能用的list的fuction
def SWC2Sklearn(resultlist):
    result = []
    for i in range(0, len(resultlist)):
        one = [word for word in resultlist[i]]
        result.append(' '.join(one))
    return result


# 生成TD-IDF词向量function
def TfIdf(resultlist):
    
    # tf-idf和LDA数据准备
    
    counter = CountVectorizer()
    counts = counter.fit_transform(resultlist)
    
    # tf-idf
    tfidfer = TfidfTransformer()
    tfidf = tfidfer.fit_transform(counts)
    wordVocabulary = counter.vocabulary_
    wordVector = counts.toarray()
    tfidfVector = tfidf.toarray()  
    
    # result
    result = {'tfidf_wordVoc':wordVocabulary,
              'tfidf_wordVec':wordVector,
              'tfidf_tfidfVec':tfidfVector}
    return result

# 生成TD-IDF和LDA词向量function
def LDA(resultlist, n_topics=5, n_iter=100, random_state=0,):
    
    # LDA数据准备
    
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
    result = {'lda_doctopicVec':doc_topic}
    return result




def WORD2VEC(resultlist, sg=0, vector_size=100, window=5, min_count=2, negative=3, sample=0.001, hs=0, workers=4, epochs=5):
    # word2vec
    # 准备数据
    list_2vec = []
    for i in resultlist:
        list_2vec.append(i.split())
    word2vecModel = gensim.models.word2vec.Word2Vec(list_2vec,
                                                    sg=sg, # 训练算法，默认0对应CBOW，1对应skip-gram
                                                    vector_size=vector_size, # 输出词的向量维度
                                                    window=window, # 训练窗口大小，默认5，考虑前后各5个词的影响
                                                    min_count=min_count, # 少于min_count次数的单词会被丢弃
                                                    negative=negative, # 如果>0则会采用negativesamping，用于设置多少个noise words
                                                    sample=sample, # 采样阈值，如果一个词在训练样本中出现评率越大，那么就越会被采样，范围(0, 1e-5)
                                                    hs=hs, # 是否采用HS方法，0使用，1不使用，默认0
                                                    workers=workers, # 参数控制训练的并行数
                                                    epochs=epochs) #迭代次数，默认为5
    result = word2vecModel.wv
    return result
    




################ test

# 导入数据
data_tf = pd.read_csv('原始数据.csv',
                      names=['深层现象', '故障现象', '故障分析', '处理方式'],
                      encoding='utf-8')
data_tf.columns = ['deepph', 'ph', 'analyze', 'handle']
data_tf = data_tf[1:]
data_tf.index = pd.Series(range(1, 878))

stopwords = stopwordslist(r"C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/stopwords/bd_hit_scu_stopwords.txt")
# 对故障现象(ph)进行分词
data_cut_ph = data_tf.ph
list_result_cut_ph = simpleWordCut(pd.Series.tolist(data_cut_ph.astype(str)), stopwords, cut_all=False, HMM=False)
# 提取故障现象(ph)的特征向量
list_4Feature_ph = SWC2Sklearn(list_result_cut_ph)
dic_result_CFV_ph = TfIdf(list_4Feature_ph)
tfidfVec = dic_result_CFV_ph['tfidf_tfidfVec']




