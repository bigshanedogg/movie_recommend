***README***
----------
**This project is for constructing movie recommendation system with limited data.** Only the data can be crawled in web was used such as review data - title/score/content and movie data including genre,grade,nation etc. **The purpose is finding whether various ensemble models can increase the accuracy even if the data is limited and which models fits the most relative to naive recommendation.** So, The model chosen can be elaborated given more data such as demographic data. It can be applied and even more elaborated in other recommendation system such as music streaming service which has usage log data as well as user demographic data.
After comparing methods, I could check that association rule and item-based collaborative filtering are not effective in this kind of recommendation with limited data while **ensemble model using k-means and user-based collaborative filtering shows the best accuracy, which is 190 times more accurate than naive recommendation.** 
The details of modeling and idea are in "Naver_port.pdf".


----------


**> Folder description**
> - pyhon_code : codes for crawling web data, one is for collecting movie information such as genre, director etc, and other is for
> collecting review data of each movie.
> - r_code : codes for analyzing crawled data such as k-means, collaborative filtering and desiging ensemble model.
> - raw_data : data crawled in web.
> - refined_data : data preprocessed to proper format to use.
> - result : recommendation result calculated by many single or ensemble method such as k-means, collaborative filtering etc.
> - result_graph : graph showing comparing the accuracy of methods.
> - simple_analysis : simple result showing relationship between review score and nation/genre/grade/director.

