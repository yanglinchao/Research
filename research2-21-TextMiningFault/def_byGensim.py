# -*- coding: utf-8 -*-
"""
Created on Mon Jul 12 00:02:09 2021

@author: ylc
"""

import os
import pandas as pd
import numpy as np
import jieba
from gensim import corpora, models


###################### function

# 创建停用词function
def stopwordslist(stopwordsFilePath):  
    stopwords = [line.strip() for line in open(stopwordsFilePath, 'r', encoding='utf-8').readlines()]  
    return stopwords

# 检查停用词function
def checkStopwords(resultFromCut, stopwords):
    outstr = []
    for word in resultFromCut:
        if word not in stopwords:
            if word != '\t':
                outstr.append(word)
    return outstr

# 创建分词function
def simpleWordCut(list_4wordcut, stopwords, cut_all=False, HMM=False):
    result = []
    for i in range(0, len(list_4wordcut)):
        result_onecut = []
        onecut = jieba.cut(list_4wordcut[i], cut_all=cut_all, HMM=HMM)
        onecut = checkStopwords(onecut, stopwords)
        for j in onecut:          
            result_onecut.append(j)
        result.append(result_onecut)
    return result


# 创建tf-idf特征矩阵
def tfidf_Gensim(list_result_wordcut):
    
    dictionary = corpora.Dictionary(list_result_wordcut)
    
    # 得到每一篇文档对应的稀疏向量（指bow向量）
    # 向量的每一个元素代表了一个word在这篇文档中出现的次数
    corpus = [dictionary.doc2bow(text) for text in list_result_wordcut]
    
    # 转化为tf-idf向量
    # corpus是一个返回bow向量的迭代器。下面将完成对corpus中出现的每一个特征的IDF值的统计工作
    tfidf_model = models.TfidfModel(corpus)
    corpus_tfidf = [tfidf_model[doc] for doc in corpus]
    result = []
    for i in range(len(corpus_tfidf)):
        a = corpus_tfidf[i]
        singleresult = []
        for j in range(len(a)):
            singleresult.append(a[j][1])
        result.append(singleresult)
    
    return result
    

# 创建LSI特征向量
def LSI_Gensim(list_result_wordcut, tfidf=False, num_topics=5):
    
    dictionary = corpora.Dictionary(list_result_wordcut)
    
    # 得到每一篇文档对应的稀疏向量（指bow向量）
    # 向量的每一个元素代表了一个word在这篇文档中出现的次数
    corpus = [dictionary.doc2bow(text) for text in list_result_wordcut]
    
    # 建立LSI模型
    if tfidf == True:
        tfidf_model = models.TfidfModel(corpus)
        tfidf_corpus = tfidf_model[corpus]
        lsi_model = models.LsiModel(tfidf_corpus, id2word=dictionary, num_topics=num_topics)
    else:
        lsi_model = models.LsiModel(corpus, id2word=dictionary, num_topics=num_topics)    

    # 提取LSI特征向量
    corpus_lsi = [lsi_model[doc] for doc in corpus]
    result = []
    for i in range(len(corpus_lsi)):
        a = corpus_lsi[i]
        single_result = []
        for j in range(len(a)):
            single_result.append(a[j][1])
        result.append(single_result)
    
    return result



# 创建LDA特征向量
def LDA_Gensim(list_result_wordcut, tfidf=False, num_topics=5):
    
    dictionary = corpora.Dictionary(list_result_wordcut)
    
    # 得到每一篇文档对应的稀疏向量（指bow向量）
    # 向量的每一个元素代表了一个word在这篇文档中出现的次数
    corpus = [dictionary.doc2bow(text) for text in list_result_wordcut]
    
    # 建立LSI模型
    if tfidf == True:
        tfidf_model = models.TfidfModel(corpus)
        tfidf_corpus = tfidf_model[corpus]
        lda_model = models.LdaModel(tfidf_corpus, id2word=dictionary, num_topics=num_topics)
    else:
        lda_model = models.LdaModel(corpus, id2word=dictionary, num_topics=num_topics)    

    # 提取LSI特征向量
    corpus_lda = [lda_model[doc] for doc in corpus]
    result = []
    for i in range(len(corpus_lda)):
        a = corpus_lda[i]
        single_result = []
        for j in range(len(a)):
            single_result.append(a[j][1])
        result.append(single_result)
    
    return result


# 创建word2vec模型
def WORD2VEC_Gensim(list_result_wordcut, sg=0, vector_size=100, window=5, min_count=0, negative=3, sample=0.001, hs=0, workers=4, epochs=5):
    # word2vec
    word2vecModel = models.word2vec.Word2Vec(list_result_wordcut,
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

# 生成word2vec特征向量
def featureVec_2vec(list_result_wordcut, result_2vecModel, method="mean"):
    result = []
    for i in range(len(list_result_wordcut)):
        single_result = []
        for word in list_result_wordcut[i]:
            if method == "mean":
                single_result.append(result_2vecModel[word].mean())
            elif method == "max":
                single_result.append(result_2vecModel[word].max())
            elif method == "min":
                single_result.append(result_2vecModel[word].min())
        result.append(single_result)
    return result