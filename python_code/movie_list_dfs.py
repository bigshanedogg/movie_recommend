#-*- coding: utf-8 -*-
import urllib.request
import requests
import csv
from bs4 import BeautifulSoup

m_url = []
movie_info = []
movie_key = 0

dir = '/Users/hodong/Desktop/data_project/naver_movie/movie_code_u.txt'
code_file = open(dir,"r")
while True:
    line = code_file.readline()
    if not line: break
    m_url.append(line[:-1])
code_file.close()
movie_list = set(m_url)

print('수집할 영화 정보의 수 : ',len(movie_list),"\n",sep='')

per_check = [90,80,70,60,50,40,30,20,10]
ii = 0
e_count = 0
for mv in movie_list:
    ii += 1
    if len(per_check)!=0 and int(ii/len(movie_list)*100) >= per_check[-1] :
        print("현재 ",ii, "번째 영화 수집 중...", sep='')
        print(int(ii/len(movie_list)*100),"% 수집 완료...",sep='')
        per_check.pop()

    try:
        url2 = "http://movie.naver.com/movie/bi/mi/basic.nhn?code=" + mv
        movie_page = urllib.request.Request(url2)
        movie_page = urllib.request.urlopen(movie_page)
        movie_data = BeautifulSoup(movie_page, 'html.parser', from_encoding='utf-8')


        # 제목 / 장르 / 국가 / 상영시간 / 개봉일 / 등급 / 관객평점 / 비평가평점 / 네티즌 평점
        genre = 'NA' ; nation = 'NA' ; time = 'NA'; date_m = 'NA' ; grade = 'NA' ; audience = 'NA' ; critic = 'NA' ; netizen = 'NA'
        title = movie_data.find_all('div', {'mv_info'})[0].find_all('h3', {'h_movie'})[0].find_all('a')[0].get_text()
        genre = movie_data.find_all('dl', {'info_spec'})[0].find_all('dd')[0].find_all('p')[0].find_all('span')[0].find_all('a')
        nation = movie_data.find_all('dl', {'info_spec'})[0].find_all('dd')[0].find_all('p')[0].find_all('span')[1].find_all('a')[0].get_text()
        time = movie_data.find_all('dl', {'info_spec'})[0].find_all('dd')[0].find_all('p')[0].find_all('span')[2].get_text()
        grade = movie_data.find_all('dl', {'info_spec'})[0].find_all('dd')[3].find_all('p')[0].find_all('a')[0].get_text()
        date_m = movie_data.find_all('dl', {'info_spec'})[0].find_all('dd')[0].find_all('p')[0].find_all('span')[3].find_all('a')
        date = date_m[0].get_text() + date_m[1].get_text()

        actor = movie_data.find_all('dl', {'info_spec'})[0].find_all('dd')[2].find_all('p')[0].find_all('a')
        actor_list = []
        for act in actor :
            actor_list.append(act.get_text())

        director = movie_data.find_all('dl', {'info_spec'})[0].find_all('dd')[1].find_all('p')[0].find_all('a')[0].get_text()
        genre_list = []
        for gen in genre :
            #genre_list.append(gen.get_text())
            genre_list.append(gen.get_text())



        # 평점 구하기 aud_score, cri_score, net_score
        temp = movie_data.find_all('div', {'main_score'})
        temp2 = temp[0].find_all('div',{'score'})

        audience = temp2[0].find_all('div',{'star_score'})[0].find_all('em')
        aud_score = ''
        for aud in audience :
            aud_score += aud.get_text()

        critic = temp2[1].find_all('div', {'star_score'})[0].find_all('em')
        cri_score = ''
        for cri in critic :
            cri_score += cri.get_text()

        netizen = temp2[2].find_all('div', {'star_score'})[0].find_all('em')
        net_score = ''
        for net in netizen :
            net_score += net.get_text()

        #print("제목 : ", title,", 감독 : ",director,", 등급 : ",grade,", 배우 : ",actor_list,", 장르 : ",genre_list,", 국가 : ",nation,", 시간 : ",time,", 개봉일 : ",date)
        #print("관객 평점 : ",aud_score,", 비평가 평점 : ",cri_score,", 네티즌 펴점 : ",net_score)
        movie_key+=1
        movie_info.append([movie_key,title, genre_list, nation, time, date, grade, director, actor_list, aud_score, cri_score, net_score])

    except IndexError:
        print('IndexError in extracting movie_list')
        e_count +=1


#--------------------------------------#영화 정보 파일에 저장#--------------------------------------#
dir = '/Users/hodong/Desktop/data_project/naver_movie/movie_list_u.csv'  # 디렉토리 설정 필요
with open(dir, 'w', encoding='utf-8') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    for movie in movie_info:
        writer.writerow(movie)

print('\n',e_count,"개의 에러 발생...",sep='')
print('총 ',len(movie_list)-e_count,"개의 영화 리스트 정보 수집 완료...!",sep='')

