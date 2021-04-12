# -*- coding: utf-8 -*-
"""
Created on Sun Apr 11 18:30:11 2021

@author: USER
"""

seoul = ['Jane','Kim']
def solution(seoul):
    where = seoul.index('Kim')
    answer = ('김서방은 %d에 있다' %where)
    return answer
solution(seoul)