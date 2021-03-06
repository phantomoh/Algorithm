# -*- coding: utf-8 -*-
"""
Created on Tue May  4 22:48:02 2021

@author: USER
"""

{
 "cells": [
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "# 숫자 손글씨 분류하기\n",
    "\n",
    "--\n",
    "## 문제 정의\n",
    "28x28 픽셀의 손글씨 숫자 이미지를 입력 받아서 실제로 의미하는 숫자를 인식해보자\n",
    "\n",
    "## 가설\n",
    "784개의 특징 데이터를 구성한 후 머신러닝 분석을 한 후 어떤 숫자인지 추측이 가능\n",
    "\n",
    "## 목표\n",
    "28x28 사이즈의 이미지로부터 label값을 얻어낸다"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": []
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "# 데이터 구성\n",
    "784개 입력특징(28x28픽셀)\n",
    "**출력 데이터** label\n",
    "데이터의 총 행수 10000개\n",
    "\n",
    "# 분석단계\n",
    "## 단계1) 데이터를 불러온다\n",
    "## 단계2) EDA & Feature Engnieering\n",
    "## 단계3) 데이터셋 구성한다\n",
    "## 단계4) 모델링하고 학습\n",
    "## 단계5) 모델 검증"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "---\n",
    "1. 데이터를 불러오기"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 1,
   "metadata": {},
   "outputs": [],
   "source": [
    "import pandas as pd\n",
    "import numpy as np"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<div>\n",
       "<style scoped>\n",
       "    .dataframe tbody tr th:only-of-type {\n",
       "        vertical-align: middle;\n",
       "    }\n",
       "\n",
       "    .dataframe tbody tr th {\n",
       "        vertical-align: top;\n",
       "    }\n",
       "\n",
       "    .dataframe thead th {\n",
       "        text-align: right;\n",
       "    }\n",
       "</style>\n",
       "<table border=\"1\" class=\"dataframe\">\n",
       "  <thead>\n",
       "    <tr style=\"text-align: right;\">\n",
       "      <th></th>\n",
       "      <th>pixel 1,1</th>\n",
       "      <th>pixel 1,2</th>\n",
       "      <th>pixel 1,3</th>\n",
       "      <th>pixel 1,4</th>\n",
       "      <th>pixel 1,5</th>\n",
       "      <th>pixel 1,6</th>\n",
       "      <th>pixel 1,7</th>\n",
       "      <th>pixel 1,8</th>\n",
       "      <th>pixel 1,9</th>\n",
       "      <th>pixel 1,10</th>\n",
       "      <th>...</th>\n",
       "      <th>pixel 28,20</th>\n",
       "      <th>pixel 28,21</th>\n",
       "      <th>pixel 28,22</th>\n",
       "      <th>pixel 28,23</th>\n",
       "      <th>pixel 28,24</th>\n",
       "      <th>pixel 28,25</th>\n",
       "      <th>pixel 28,26</th>\n",
       "      <th>pixel 28,27</th>\n",
       "      <th>pixel 28,28</th>\n",
       "      <th>label</th>\n",
       "    </tr>\n",
       "  </thead>\n",
       "  <tbody>\n",
       "    <tr>\n",
       "      <th>0</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>4</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>1</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>8</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>2</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>8</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>3</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>7</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>4</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>4</td>\n",
       "    </tr>\n",
       "  </tbody>\n",
       "</table>\n",
       "<p>5 rows × 785 columns</p>\n",
       "</div>"
      ],
      "text/plain": [
       "   pixel 1,1  pixel 1,2  pixel 1,3  pixel 1,4  pixel 1,5  pixel 1,6  \\\n",
       "0        0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "1        0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "2        0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "3        0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "4        0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "\n",
       "   pixel 1,7  pixel 1,8  pixel 1,9  pixel 1,10  ...  pixel 28,20  pixel 28,21  \\\n",
       "0        0.0        0.0        0.0         0.0  ...          0.0          0.0   \n",
       "1        0.0        0.0        0.0         0.0  ...          0.0          0.0   \n",
       "2        0.0        0.0        0.0         0.0  ...          0.0          0.0   \n",
       "3        0.0        0.0        0.0         0.0  ...          0.0          0.0   \n",
       "4        0.0        0.0        0.0         0.0  ...          0.0          0.0   \n",
       "\n",
       "   pixel 28,22  pixel 28,23  pixel 28,24  pixel 28,25  pixel 28,26  \\\n",
       "0          0.0          0.0          0.0          0.0          0.0   \n",
       "1          0.0          0.0          0.0          0.0          0.0   \n",
       "2          0.0          0.0          0.0          0.0          0.0   \n",
       "3          0.0          0.0          0.0          0.0          0.0   \n",
       "4          0.0          0.0          0.0          0.0          0.0   \n",
       "\n",
       "   pixel 28,27  pixel 28,28  label  \n",
       "0          0.0          0.0      4  \n",
       "1          0.0          0.0      8  \n",
       "2          0.0          0.0      8  \n",
       "3          0.0          0.0      7  \n",
       "4          0.0          0.0      4  \n",
       "\n",
       "[5 rows x 785 columns]"
      ]
     },
     "execution_count": 2,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "df = pd.read_csv('data/digit.csv')\n",
    "df.head()"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## 2. EDA(데이터 탐색)& Feature Engineering"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "metadata": {},
   "outputs": [],
   "source": [
    "import matplotlib.pyplot as plt\n",
    "\n",
    "import seaborn as sns\n",
    "sns.set()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<div>\n",
       "<style scoped>\n",
       "    .dataframe tbody tr th:only-of-type {\n",
       "        vertical-align: middle;\n",
       "    }\n",
       "\n",
       "    .dataframe tbody tr th {\n",
       "        vertical-align: top;\n",
       "    }\n",
       "\n",
       "    .dataframe thead th {\n",
       "        text-align: right;\n",
       "    }\n",
       "</style>\n",
       "<table border=\"1\" class=\"dataframe\">\n",
       "  <thead>\n",
       "    <tr style=\"text-align: right;\">\n",
       "      <th></th>\n",
       "      <th>pixel 1,1</th>\n",
       "      <th>pixel 1,2</th>\n",
       "      <th>pixel 1,3</th>\n",
       "      <th>pixel 1,4</th>\n",
       "      <th>pixel 1,5</th>\n",
       "      <th>pixel 1,6</th>\n",
       "      <th>pixel 1,7</th>\n",
       "      <th>pixel 1,8</th>\n",
       "      <th>pixel 1,9</th>\n",
       "      <th>pixel 1,10</th>\n",
       "      <th>...</th>\n",
       "      <th>pixel 28,20</th>\n",
       "      <th>pixel 28,21</th>\n",
       "      <th>pixel 28,22</th>\n",
       "      <th>pixel 28,23</th>\n",
       "      <th>pixel 28,24</th>\n",
       "      <th>pixel 28,25</th>\n",
       "      <th>pixel 28,26</th>\n",
       "      <th>pixel 28,27</th>\n",
       "      <th>pixel 28,28</th>\n",
       "      <th>label</th>\n",
       "    </tr>\n",
       "  </thead>\n",
       "  <tbody>\n",
       "    <tr>\n",
       "      <th>count</th>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>...</td>\n",
       "      <td>10000.000000</td>\n",
       "      <td>10000.000000</td>\n",
       "      <td>10000.000000</td>\n",
       "      <td>10000.000000</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.0</td>\n",
       "      <td>10000.000000</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>mean</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.000480</td>\n",
       "      <td>0.000239</td>\n",
       "      <td>0.000050</td>\n",
       "      <td>0.000025</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>4.453400</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>std</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.017804</td>\n",
       "      <td>0.013588</td>\n",
       "      <td>0.003535</td>\n",
       "      <td>0.002500</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>2.884451</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>min</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.000000</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>25%</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>2.000000</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>50%</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>4.000000</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>75%</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.000000</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>7.000000</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>max</th>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>...</td>\n",
       "      <td>0.988281</td>\n",
       "      <td>0.988281</td>\n",
       "      <td>0.250000</td>\n",
       "      <td>0.250000</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>0.0</td>\n",
       "      <td>9.000000</td>\n",
       "    </tr>\n",
       "  </tbody>\n",
       "</table>\n",
       "<p>8 rows × 785 columns</p>\n",
       "</div>"
      ],
      "text/plain": [
       "       pixel 1,1  pixel 1,2  pixel 1,3  pixel 1,4  pixel 1,5  pixel 1,6  \\\n",
       "count    10000.0    10000.0    10000.0    10000.0    10000.0    10000.0   \n",
       "mean         0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "std          0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "min          0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "25%          0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "50%          0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "75%          0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "max          0.0        0.0        0.0        0.0        0.0        0.0   \n",
       "\n",
       "       pixel 1,7  pixel 1,8  pixel 1,9  pixel 1,10  ...   pixel 28,20  \\\n",
       "count    10000.0    10000.0    10000.0     10000.0  ...  10000.000000   \n",
       "mean         0.0        0.0        0.0         0.0  ...      0.000480   \n",
       "std          0.0        0.0        0.0         0.0  ...      0.017804   \n",
       "min          0.0        0.0        0.0         0.0  ...      0.000000   \n",
       "25%          0.0        0.0        0.0         0.0  ...      0.000000   \n",
       "50%          0.0        0.0        0.0         0.0  ...      0.000000   \n",
       "75%          0.0        0.0        0.0         0.0  ...      0.000000   \n",
       "max          0.0        0.0        0.0         0.0  ...      0.988281   \n",
       "\n",
       "        pixel 28,21   pixel 28,22   pixel 28,23  pixel 28,24  pixel 28,25  \\\n",
       "count  10000.000000  10000.000000  10000.000000      10000.0      10000.0   \n",
       "mean       0.000239      0.000050      0.000025          0.0          0.0   \n",
       "std        0.013588      0.003535      0.002500          0.0          0.0   \n",
       "min        0.000000      0.000000      0.000000          0.0          0.0   \n",
       "25%        0.000000      0.000000      0.000000          0.0          0.0   \n",
       "50%        0.000000      0.000000      0.000000          0.0          0.0   \n",
       "75%        0.000000      0.000000      0.000000          0.0          0.0   \n",
       "max        0.988281      0.250000      0.250000          0.0          0.0   \n",
       "\n",
       "       pixel 28,26  pixel 28,27  pixel 28,28         label  \n",
       "count      10000.0      10000.0      10000.0  10000.000000  \n",
       "mean           0.0          0.0          0.0      4.453400  \n",
       "std            0.0          0.0          0.0      2.884451  \n",
       "min            0.0          0.0          0.0      0.000000  \n",
       "25%            0.0          0.0          0.0      2.000000  \n",
       "50%            0.0          0.0          0.0      4.000000  \n",
       "75%            0.0          0.0          0.0      7.000000  \n",
       "max            0.0          0.0          0.0      9.000000  \n",
       "\n",
       "[8 rows x 785 columns]"
      ]
     },
     "execution_count": 4,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "df.describe()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 5,
   "metadata": {
    "scrolled": false
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "<seaborn.axisgrid.FacetGrid at 0x20f1cc67520>"
      ]
     },
     "execution_count": 5,
     "metadata": {},
     "output_type": "execute_result"
    },
    {
     "data": {
      "image/png": "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\n",
      "text/plain": [
       "<Figure size 360x360 with 1 Axes>"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "sns.catplot(data=df, x='label', kind='count')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 6,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "<seaborn.axisgrid.FacetGrid at 0x20f20d1a220>"
      ]
     },
     "execution_count": 6,
     "metadata": {},
     "output_type": "execute_result"
    },
    {
     "data": {
      "image/png": "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\n",
      "text/plain": [
       "<Figure size 1440x360 with 1 Axes>"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "sns.catplot(data=df, x='label', kind='box', aspect=4) \n",
    "# 박스플랏 \n",
    "# aspect : 크기를 늘림 "
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## 이미지 살펴보기"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 7,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "image/png": "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\n",
      "text/plain": [
       "<Figure size 432x288 with 1 Axes>"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "numbers = df.drop(['label'], axis = 1)\n",
    "\n",
    "nth = 0 # 0~9999 바꾸시면서 살펴보셔요\n",
    "img = np.reshape(numbers.iloc[nth].values, [28, 28]) #iloc[2] : 2행을 가져온다 \n",
    "plt.imshow(img)\n",
    "plt.show()"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "---\n",
    "# 3. data set 구성하기"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 8,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "(10000, 784) (10000,)\n"
     ]
    }
   ],
   "source": [
    "input_data = df.drop('label', axis = 1)\n",
    "target_data = df['label']\n",
    "\n",
    "print(input_data.shape, target_data.shape)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 9,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "(8000, 784) (2000, 784)\n"
     ]
    }
   ],
   "source": [
    "from sklearn.model_selection import train_test_split\n",
    "x_train, x_test, y_train, y_test = train_test_split(input_data, target_data, test_size=0.2)\n",
    "\n",
    "print(x_train.shape, x_test.shape)"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## validation set 만들기\n",
    "### 모델 제작 과정 중, 학습된 모델의 성능을 측정하기 위한 데이터"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 10,
   "metadata": {},
   "outputs": [],
   "source": [
    "x_valid = x_train[0:1600] # 0~1600번까지 \n",
    "y_valid = y_train[0:1600]\n",
    "\n",
    "x_train = x_train[1600:] # 1600번부터 끝까지 \n",
    "y_train = y_train[1600:]"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "---\n",
    "# 4. 모델링 & 학습"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## Random Forest"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 11,
   "metadata": {},
   "outputs": [],
   "source": [
    "from sklearn.ensemble import RandomForestClassifier\n",
    "forest = RandomForestClassifier(n_estimators = 100)\n",
    "#n_estimators : 나무의 갯수, 얼마나 수정할 지 "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 12,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "RandomForestClassifier()"
      ]
     },
     "execution_count": 12,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "forest.fit(x_train, y_train) # 데이터 학습"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 13,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "training set accuracy: 1.0\n",
      "validation set accuracy:  0.95375\n"
     ]
    }
   ],
   "source": [
    "print('training set accuracy:', forest.score(x_train, y_train)) # 학습 1번 시키면 100% 맞춤\n",
    "print('validation set accuracy: ', forest.score(x_valid, y_valid))"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## SVM"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 14,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "SVC()"
      ]
     },
     "execution_count": 14,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "from sklearn.svm import SVC\n",
    "model = SVC()\n",
    "model.fit(x_train, y_train)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 15,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "training set accuracy: 0.98484375\n",
      "validation set accuracy: 0.968125\n"
     ]
    }
   ],
   "source": [
    "print('training set accuracy:', model.score(x_train, y_train))\n",
    "print('validation set accuracy:', model.score(x_valid, y_valid))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": []
  },
  {
   "cell_type": "code",
   "execution_count": 22,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "[0.958 0.955 0.955 0.95  0.964 0.962 0.962 0.965 0.957 0.963]\n",
      "0.9591\n"
     ]
    }
   ],
   "source": [
    "from sklearn.model_selection import KFold, cross_val_score\n",
    "kfold = KFold(n_splits=10, shuffle=True)\n",
    "score = cross_val_score(model, input_data, target_data.values.ravel(), cv=kfold)\n",
    "print(score)\n",
    "print(score.mean())"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "---\n",
    "# 5. 모델 검증"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 16,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "RT: test set accuracy: 0.9425\n"
     ]
    }
   ],
   "source": [
    "print('RT: test set accuracy:', forest.score(x_test, y_test))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 17,
   "metadata": {
    "scrolled": true
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "SVM: test set accuracy: 0.9565\n"
     ]
    }
   ],
   "source": [
    "print('SVM: test set accuracy:', model.score(x_test, y_test))"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## 실제 예측 결과물 살펴보기"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 18,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "image/png": "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\n",
      "text/plain": [
       "<Figure size 432x288 with 1 Axes>"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "---인식된 숫자는? [5] 입니다\n"
     ]
    },
    {
     "data": {
      "image/png": "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\n",
      "text/plain": [
       "<Figure size 432x288 with 1 Axes>"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "---인식된 숫자는? [6] 입니다\n"
     ]
    },
    {
     "data": {
      "image/png": "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\n",
      "text/plain": [
       "<Figure size 432x288 with 1 Axes>"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "---인식된 숫자는? [5] 입니다\n"
     ]
    },
    {
     "data": {
      "image/png": "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\n",
      "text/plain": [
       "<Figure size 432x288 with 1 Axes>"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "---인식된 숫자는? [7] 입니다\n"
     ]
    }
   ],
   "source": [
    "import random\n",
    "\n",
    "for i in range(4):\n",
    "    n = random.randrange(0, len(x_test))\n",
    "    \n",
    "    img = np.reshape(x_test.iloc[n].values, [28,28])\n",
    "    plt.imshow(img)\n",
    "    plt.show()\n",
    "\n",
    "    result = forest.predict([x_test.iloc[n].values]) # 예측할때는 predict\n",
    "    print('---인식된 숫자는?', result, '입니다')"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.8.3"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 2
}
