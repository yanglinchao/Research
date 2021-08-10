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
def simpleWordCut(list_4wordcut, stopwords=False, cut_all=False, HMM=False):
    # stopwords:是否使用停用词表，默认False，如果传入停用词表，则直接使用
    # cut_all:是否使用全模式（把句子中所有可以成词的词语都扫描出来），默认False是使用精确模式（将句子最精确地切开，适合文本分析）
    # HMM：是否使用HMM模型（便于新词发现），默认False
    result = []
    for i in range(0, len(list_4wordcut)):
        result_onecut = []
        onecut = jieba.cut(list_4wordcut[i], cut_all=cut_all, HMM=HMM)
        if stopwords == False:
            for j in onecut:
                result_onecut.append(j)
        else:
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
    
    corpus_tfidf = list(tfidf_model[corpus])
    len_dictionary = len(dictionary)
    result = []
    for i in range(len(corpus_tfidf)):
        # 生成一个和字典长度相等的向量
        result_one = [0]*len_dictionary
        # 提取第i个文档的corpus_tfidf
        corpus_tfidf_i = corpus_tfidf[i]
        for j in range(len(corpus_tfidf_i)):
            # 提取该文档的第j个单词的corpus_tfidf
            doc_corpus_tfidf = corpus_tfidf_i[j] # 位置0是单词在result_one中的位置，位置1是单词的tf-idf值
            site = doc_corpus_tfidf[0]
            doc_tfidf = doc_corpus_tfidf[1]
            result_one[site] = doc_tfidf
        result.append(result_one)    
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
    corpus_lsi = list(lsi_model[corpus])
    result = []
    for i in range(len(corpus_lsi)):
        result_one = []
        corpus_lsi_i = corpus_lsi[i]
        for j in range(len(corpus_lsi_i)):
            result_one.append(corpus_lsi_i[j][1])
        result.append(result_one)
    
    return result



# 创建LDA特征向量
def LDA_Gensim(list_result_wordcut, tfidf=False, num_topics=5):
    
    dictionary = corpora.Dictionary(list_result_wordcut)
    
    # 得到每一篇文档对应的稀疏向量（指bow向量）
    # 向量的每一个元素代表了一个word在这篇文档中出现的次数
    corpus = [dictionary.doc2bow(text) for text in list_result_wordcut]
    
    # 建立LDA模型
    if tfidf == True:
        tfidf_model = models.TfidfModel(corpus)
        tfidf_corpus = tfidf_model[corpus]
        lda_model = models.LdaModel(tfidf_corpus, id2word=dictionary, num_topics=num_topics)
    else:
        lda_model = models.LdaModel(corpus, id2word=dictionary, num_topics=num_topics)    

    # 提取LDA特征向量
    corpus_lda = list(lda_model[corpus])
    result = []
    for i in range(len(corpus_lda)):
        # 生成一个和字典长度相等的向量
        result_one = [0]*num_topics
        # 提取第i个文档的corpus_lda
        corpus_lda_i = corpus_lda[i]
        for j in range(len(corpus_lda_i)):
            # 提取该文档的第j个单词的corpus_lda
            doc_corpus_lda = corpus_lda_i[j] # 位置0是单词在result_one中的位置，位置1是单词的tf-idf值
            site = doc_corpus_lda[0]
            doc_lda = doc_corpus_lda[1]
            result_one[site] = doc_lda
        result.append(result_one) 
    
    return result


# 创建word2vec模型
def WORD2VEC_Gensim(list_result_wordcut, sg=0, vector_size=100, window=5, min_count=0, negative=0, sample=0.001, hs=0, workers=4, epochs=10):
    
    from gensim.models.word2vec import Word2Vec
    
    # word2vec
    word2vecModel = Word2Vec(list_result_wordcut,
                             vector_size=vector_size, # 输出词的向量维度
                             window=window, # 训练窗口大小，默认5，考虑前后各5个词的影响
                             min_count=min_count, # 少于min_count次数的单词会被丢弃
                              workers=workers, # 参数控制训练的并行数
                             sg=sg, # 训练算法，默认0对应CBOW，1对应skip-gram
                             hs=hs, # hs=0使用negative sampling; hs=1使用hierarchical softmax
                             negative=negative, # 如果>0则会采用negativesamping，用于设置多少个noise words，通常5-20
                             sample=sample, # 采样阈值，如果一个词在训练样本中出现评率越大，那么就越会被采样，范围(0, 1e-5)
                             epochs=epochs #迭代次数，默认为5
                             ) 
    result = word2vecModel.wv
    return result

# 生成word2vec特征向量
def featureVec_2vec(list_result_wordcut, mv_2vecModel):
    
    from gensim import corpora
    
    dictionary = corpora.Dictionary(list_result_wordcut)

    # 求每个单词的value
    word2vec_value = []
    for i in range(len(mv_2vecModel.vectors)):
        word2vec_value.append(mv_2vecModel.vectors[i].mean())
        
    result = []
    for i in range(len(list_result_wordcut)):
        doc = list_result_wordcut[i]
        result_one = [0]*len(mv_2vecModel.vectors)
        for j in range(len(doc)):
            # 查看文档中第j个单词在word2vec模型中的位置
            word = doc[j]
            site_word2vec = mv_2vecModel.key_to_index[word]
            # 返回该单词的word2vec的value
            value_word2vec = word2vec_value[site_word2vec]
            # 查看该单词在dictionary中的位置
            site_dictionary = dictionary.token2id[word]
            # 把该单词的word2vec值放在向量的对应位置
            result_one[site_dictionary] = value_word2vec
        result.append(result_one)
    return result


# 创建doc2vec模型
def DOC2VEC_Gensim(list_result_wordcut, dm=0, vector_size=100, window=5, min_count=0, workers=4, epochs=10, hs=0, negative=3):
    
    from gensim.models.doc2vec import Doc2Vec, TaggedDocument
    
    # 把分词后的结果转换成用于doc2vec的输入
    documents = [TaggedDocument(doc, [i]) for i, doc in enumerate(list_result_wordcut)]
    # 建立doc2vec模型
    model = Doc2Vec(documents,
                    dm=dm, # dm=0使用PV-DBOW模型，dm=1使用PV-DM模型
                    vector_size=vector_size, # 特征向量维度
                    window=window, # 训练窗口大小，当前词和预测值之间的距离
                    min_count=min_count, # 频次小于该值的词将被忽略
                    workers=workers, # 建模的并行数
                    epochs=epochs, # 迭代次数
                    hs=hs, # hs=0使用negative sampling; hs=1使用hierarchical softmax
                    negative=negative # 如果>0，将使用负采样，用于设置noise words数量
                    )
    result = model
    return model



