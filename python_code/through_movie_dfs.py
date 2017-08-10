#-*- coding: utf-8 -*-
import urllib.request
import requests
import csv
import re
from bs4 import BeautifulSoup

m_url = []
review_dic = dict()
user_key = 1
c_page = 100 #한 영화당 수집할 페이지 수
m_page = 5 #꺼내올 코드 수


def review_parse(userl) :
    global user_key
    # print("user_key : ",user_key)
    for tr in userl.find_all('tr')[1:]:  # 유저 페이지의 각 리뷰 항목 별로 받아서 시작
        review_id = tr.find_all('td', {'ac', 'num'})[0].get_text()
        if tag.get_text() in review_dic:
            if review_id in review_dic[tag.get_text()][2:][0].keys():  # 이미 조회한 아이디일 경우 break
                break
            else:  # 아이디는 있지만 해당 리뷰는 없는 경우
                score = tr.find_all('td', {'point'})[0].get_text()
                title = tr.find_all('td', {'title'})[0].find_all('a', {'movie'})[0].get_text()
                content = tr.find_all('td', {'title'})[0].br.next[:-39]
                movie = {review_id: [title, score, content]}
                review_dic[tag.get_text()].append(movie)
                review_dic[tag.get_text()][1] = int(review_dic[tag.get_text()][1])+1
        else:  # 아이디 조회가 처음인 경우
            review_dic[tag.get_text()] = [user_key, 0]
            score = tr.find_all('td', {'point'})[0].get_text()
            title = tr.find_all('td', {'title'})[0].find_all('a', {'movie'})[0].get_text()
            content = tr.find_all('td', {'title'})[0].br.next[:-39]
            movie = {review_id: [title, score, content]}
            review_dic[tag.get_text()].append(movie)
            review_dic[tag.get_text()][1] += 1
            user_key += 1

dir = '/Users/hodong/Desktop/data_project/naver_movie/movie_code_u2.txt'
code_file = open(dir,"r")
while True:
    line = code_file.readline()
    if not line: break
    m_url.append(line[:-1])
code_file.close()

print('읽어들인 코드의 수 : ',len(set(m_url)),sep='')
# --------------------------------------#영화 코드 파일 갱신#--------------------------------------#
m_url2 = set(m_url)
m_url = set(m_url[1:m_page+1])
m_url2 = m_url2 - m_url

dir = '/Users/hodong/Desktop/data_project/naver_movie/movie_code_u2.txt'  # 갱신 후 파일 저
code_file = open(dir,"w")
for movie in m_url2:
    code_file.write(movie+'\n')
code_file.close()

dir = '/Users/hodong/Desktop/data_project/naver_movie/movie_code_u3.txt'  # 에러에 대비한 임시 파일 저장
code_file = open(dir,"w")
for movie in m_url:
    code_file.write(movie+'\n')
code_file.close()


print('작업할 코드의 수 : ',len(m_url),sep='')
print('새로 업데이트된 코드의 수 : ',len(m_url2),'\n',sep='')

#
# --------------------------------------#이전 리뷰 정보 불러오기#--------------------------------------#
# 사전 형식 구성 {'ID' : [user_key, 총 리뷰수, {후기id : [제목, 평점, 후기]}]}
dir = '/Users/hodong/Desktop/data_project/naver_movie/movie_parse_u2.csv'  # 디렉토리 설정 필요
try :
    global user_key
    with open(dir, 'rt', encoding='utf-8') as csvfile :
        reader = csv.reader(csvfile)
        for row in reader :
            if int(row[0]) > user_key : user_key = int(row[0])+1
            check = False
            if row[1] in review_dic : #아이디가 이미 있는 경우
                for r_id in review_dic[row[1]][2:] :
                    if row[3] in r_id.keys() : #중에서 리뷰id도 있는 경우, 이번 행은 스킵
                        check = True
                        break
                if check :
                    continue
                else :
                    movie={row[3]:[row[4],row[5],row[6]]}
                    review_dic[row[1]].append(movie)
            else :
                movie={row[3]:[row[4],row[5],row[6]]}
                review_dic[row[1]] = [row[0],row[2],movie]
    print('이전 파일에 이어서 수집을 계속합니다.\n')
    #print('count_r : ',count_r,", user_key : ",user_key)
except FileNotFoundError :
    print('파일이 존재하지 않습니다. 첫 수집을 시작합니다.\n')

# --------------------------------------#리뷰 정보 파싱 시작#--------------------------------------#
print(len(m_url),"개의 영화에서 리뷰를 수집...\n",sep='')

per_check = [90,80,70,60,50,40,30,20,10]

ii = 0
for mu in m_url:
    ii += 1
    global user_key

    movie_r = "http://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=" + mu + "&target=after"
    movie_r_page = urllib.request.Request(movie_r)
    movie_r_page = urllib.request.urlopen(movie_r_page).read()
    user_in_movie = BeautifulSoup(movie_r_page.decode('euc-kr', 'replace'), 'html.parser', from_encoding='utf-8')
    title = user_in_movie.find_all('div', {'choice_movie_info'})[0].find_all('h5')[0].find_all('a')[0].get_text()
    print('\'',title,'\' 페이지의 리뷰 정보를 수집합니다.')
    # try:
    for i in range(1,c_page) :
        i = str(i)
        movie_r = "http://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=" + mu +"&target=after&page=" + i
        movie_r_page = urllib.request.Request(movie_r)
        movie_r_page = urllib.request.urlopen(movie_r_page).read()
        user_in_movie = BeautifulSoup(movie_r_page.decode('euc-kr', 'replace'), 'html.parser', from_encoding='utf-8')
        title = user_in_movie.find_all('div',{'choice_movie_info'})[0].find_all('h5')[0].find_all('a')[0].get_text()
        # print(title)
        r_pile = user_in_movie.find_all('table',{'list_netizen'})[0].find_all('tbody')[0].find_all('tr')
        # 유저 아이디별로 들어가기
        for pile in r_pile:
            # try:
                tag = pile.find_all('a',{'author'})[0]
                user_link = tag.get('class')

                if user_link != None and user_link[0] == "author":
                    user_page = urllib.request.Request("http://movie.naver.com/movie/point/af/list.nhn?st=nickname&sword=" + re.sub("[^0-9]", "", tag.get('href')) + "&target=after")
                    user_data = urllib.request.urlopen(user_page).read()
                    user_list = BeautifulSoup(user_data.decode('euc-kr', 'replace'), 'html.parser',from_encoding='utf-8').find_all('table', {'list_netizen'})[0]
                    review_parse(user_list)
            # except UnicodeEncodeError:
            #     print("Error in extracting review_data")
    # except IndexError:
    #     print('IndexError in DFS')
    if len(per_check) != 0 and int(ii / len(m_url) * 100) > per_check[-1]:
        print('\t',int(ii / len(m_url) * 100), "% 수집 완료...", sep='')
        per_check.pop()


# --------------------------------------#리뷰 정보 파일에 저장 + 중복행 제거#--------------------------------------#
count_r = 0
movie_id_list = set()
dir = '/Users/hodong/Desktop/data_project/naver_movie/movie_parse_u2.csv'  # 디렉토리 설정 필요
with open(dir, 'w', encoding='utf-8') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    for key in review_dic:
        for con in review_dic[key][2:]:
            for movie_id in con:
                if movie_id in movie_id_list : continue;
                writer.writerow(
                    [review_dic[key][0], key, review_dic[key][1], movie_id, con[movie_id][0], con[movie_id][1],
                     con[movie_id][2]])
                movie_id_list.add(movie_id)
                count_r += 1

print('총 ',count_r,'개의 영화 리뷰 수집 완료...!',sep='')