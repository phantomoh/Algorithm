# -*- coding: utf-8 -*-
"""
Created on Mon Apr  5 11:38:02 2021

@author: USER
"""

def solution(a, b):
    answer = 0
    if a <= b:
        for i in range(a,b+1):
            answer += i
    else:
        for i in range(b,a+1):
            answer += i
    return answer

# 입출력 예 
solution(3, 5)
solution(3, 3)
solution(5, 3)


