# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import os
import pandas as pd
import numpy as np
import jieba

os.chdir('C:\\Users\\ylc\\GitHub\\Research\\research2-21-TextMiningFault')

# 数据导入及整理

data_tf = pd.read_csv('原始数据.csv',
                      names=['深层现象', '故障现象', '故障分析', '处理方式'],
                      encoding='utf-8')
data_tf.index = pd.Series(range(1, 881))
data_tf.columns = ['deepph', 'ph', 'analyze', 'handle']
data_4wordcut = data_tf.deepph[1:].append(data_tf.ph[1:]).append(data_tf.analyze[1:]).append(data_tf.handle[1:])
data_4wordcut.dropna()
list_4wordcut = pd.Series.tolist(data_4wordcut.astype(str))


# 分词

# 先使用jieba.cut，全模式，HMM，去除停用词，目的是为了尽可能把所有词都分出来，以便后期筛选建立词典

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
def simpleWordCut(list4wordcut, stopwords, cut_all=True, HMM=True):
    result = []
    for i in range(0, len(list4wordcut)):
        onecut = jieba.cut(list4wordcut[i], cut_all=cut_all, HMM=HMM)
        onecut = checkStopwords(onecut, stopwords)
        for j in onecut:          
            result.append(j)
    return result

stopwords = stopwordslist(r"C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/stopwords/bd_hit_scu_stopwords.txt")
list_result_simpleWordCut = simpleWordCut(list_4wordcut, stopwords)
# data_resultSimpleWordCut = pd.DataFrame(list_result_simpleWordCut)
# data_resultSimpleWordCut = data_resultSimpleWordCut.drop_duplicates()
data_resultSimpleWordCut = pd.DataFrame(list_result_simpleWordCut)
data_resultSimpleWordCut.to_csv("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/resultWC4dict.csv", header=False, index=False)
