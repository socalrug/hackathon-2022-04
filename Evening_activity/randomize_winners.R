library(data.table)

q1 <- data.table::data.table(
         Full_name = c("Addison","Akshay Ganesh",
                       "Akshay Harish","Amit Patel","Andrew Wu","Andrew Wang",
                       "Ann Chittilappilly","Arjun Chanda","Bhargava Avancha",
                       "Bichthuy Ngo","Bijean Ghafouri","Brent Chamberlain",
                       "Carlie Wade","Chih-Han Chi","Chirag Madhukar",
                       "Chloe Ko","Christopher Chen","Chung En Chueh","Daniel Nguyen",
                       "Devin Reimers","Eric Lambert","Erica Grabowski",
                       "Faizan Haque","Flavio Wang","Forrest Chaffee",
                       "Garvit Gupta","Gary Vartanian","Gavin Hernandez",
                       "Gwendolyn Lind","Hariharan Arulmozhi","Harinishri Srikanth",
                       "Hiep Do","Horace Tsai","Jacob Wikert","Jai Agrawal",
                       "Jay Mantuhac","Jessica Romero","Jiwon Ko","Kevin Salger",
                       "Lalitha Narasimha","Lucas Morgan","Luke Morris",
                       "Mandy Huang","Martin Bach","Mehul Kotadia","Michelle Ng",
                       "Mohan Ganesan","Monica Venturini","Natalie Fenkner",
                       "Nathaniel Grelling","Neha Agrawal","Nguyen Huynh",
                       "Parth Parsana","Piyashi Biswas","Piyu Tiwari",
                       "Prashasti Sharma","Pratheek Praveen Kumar","Praveen Aravindar",
                       "Rohit Cherian Bijoy","Sakshi Mehta","San Zhang",
                       "Shan Gao","Sharvee Joshi","Shih Ting Weng","Shruti Pingale",
                       "Sneha Banerjee","Stephanie Wells","Stephanie Feldman",
                       "Sunny Zhou","Taeho Lee","Ted Alexander",
                       "Tianyuan Shao","Tom Phillips","Trung Nguyen","Veronica Swanson",
                       "Weifeng Li","Yannick van Bodegom","Yaohui Wu",
                       "Yiwen Rao","Youngjoo Ryu","Ysabel Gonzalez Rico","Yue Weng",
                       "Zhaoxuan Li"),
             Total = c(6L,4L,4L,4L,6L,4L,7L,4L,
                       14L,8L,4L,6L,7L,7L,6L,4L,8L,4L,4L,8L,4L,5L,
                       4L,4L,4L,7L,7L,5L,4L,6L,6L,7L,4L,5L,4L,4L,
                       4L,10L,5L,4L,4L,5L,7L,4L,5L,5L,6L,7L,6L,4L,
                       6L,10L,12L,4L,4L,4L,4L,4L,7L,8L,7L,4L,4L,4L,
                       4L,4L,5L,4L,4L,4L,4L,4L,4L,4L,7L,7L,4L,5L,
                       4L,4L,6L,8L,6L)
      )




# Weighted ramdomized sample

set.seed(22345657)

Winners <- sample(seq_len(nrow(q1)), 3, prob=q1$Total) #select 3
new_data <- q1[Winners, ]
