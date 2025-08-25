# race_probabilities() output hasn't changed

    structure(list(name = c("lopez", "jackson", "smith", "chan"), 
        year = c(2020, 2020, 2020, 2020), state = c("VT", "OK", "WA", 
        "NC"), county = c("chittenden", "tulsa", "king", "wake"), 
        calibisg_aian = c(0.00444499293612104, 0.0865198796172794, 
        0.0128805826771118, 0.00010959346581308), bisg_aian = c(0.0043254372504238, 
        0.085706741255251, 0.00790690484565127, 0.00013328779713989
        ), calibisg_api = c(0.0601409377957273, 0.00024340136434384, 
        0.00725714551754538, 0.970454984877748), bisg_api = c(0.0321881051000928, 
        0.00247960083768865, 0.019629623969696, 0.932059766770018
        ), calibisg_black_nh = c(0.00056859873351467, 0.36738985303624, 
        0.138842891999807, 0.00308193552380715), bisg_black_nh = c(0.0059794334503795, 
        0.437046518988705, 0.144887416123048, 0.00835292922521302
        ), calibisg_hispanic = c(0.626410161004443, 0.0261435348724924, 
        0.0148025512086416, 0.0146463076400229), bisg_hispanic = c(0.626333990237397, 
        0.0200353412280451, 0.0159140226673941, 0.02057604953591), 
        calibisg_white_nh = c(0.303816045869699, 0.478357661221058, 
        0.802940394633816, 0.00765046979828608), bisg_white_nh = c(0.319047021114721, 
        0.38484390426569, 0.770937323755778, 0.0201238342473797), 
        calibisg_other = c(0.00461926366049439, 0.0413456698885861, 
        0.0232764339630785, 0.00405670869432229), bisg_other = c(0.0121260128469858, 
        0.0698878934246207, 0.0407247086384323, 0.0187541324243397
        ), in_census = c(TRUE, TRUE, TRUE, TRUE)), class = c("compare_calibisg", 
    "data.frame"), row.names = c(NA, 4L))

# print.compare_calibisg() prints correctly

    Code
      print(out)
    Output
      Surname:  Lopez     
      State:    VT        
      County:   Chittenden
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.00         0.00      
      API        0.06         0.03      
      Black      0.00         0.01      
      Hispanic   0.63         0.63      
      White      0.30         0.32      
      Other      0.00         0.01      
      ---------------------------------------- 
      
      Surname:  Jackson   
      State:    VT        
      County:   Windsor   
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.01         0.01      
      API        0.00         0.00      
      Black      0.01         0.05      
      Hispanic   0.00         0.00      
      White      0.96         0.89      
      Other      0.02         0.05      
      ---------------------------------------- 
      
      Surname:  Smith     
      State:    WA        
      County:   King      
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.01         0.01      
      API        0.01         0.02      
      Black      0.14         0.14      
      Hispanic   0.01         0.02      
      White      0.80         0.77      
      Other      0.02         0.04      
      ---------------------------------------- 
      

---

    Code
      print(out, digits = 4)
    Output
      Surname:  Lopez     
      State:    VT        
      County:   Chittenden
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.0044       0.0043    
      API        0.0601       0.0322    
      Black      0.0006       0.0060    
      Hispanic   0.6264       0.6263    
      White      0.3038       0.3190    
      Other      0.0046       0.0121    
      ---------------------------------------- 
      
      Surname:  Jackson   
      State:    VT        
      County:   Windsor   
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.0065       0.0055    
      API        0.0020       0.0009    
      Black      0.0059       0.0489    
      Hispanic   0.0042       0.0040    
      White      0.9600       0.8932    
      Other      0.0214       0.0474    
      ---------------------------------------- 
      
      Surname:  Smith     
      State:    WA        
      County:   King      
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.0129       0.0079    
      API        0.0073       0.0196    
      Black      0.1388       0.1449    
      Hispanic   0.0148       0.0159    
      White      0.8029       0.7709    
      Other      0.0233       0.0407    
      ---------------------------------------- 
      

---

    Code
      print(out)
    Output
      Surname:  Lopez     
      State:    VT        
      County:   Chittenden
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.004        0.004     
      API        0.060        0.032     
      Black      0.001        0.006     
      Hispanic   0.626        0.626     
      White      0.304        0.319     
      Other      0.005        0.012     
      ---------------------------------------- 
      
      Surname:  Jackson   
      State:    VT        
      County:   Windsor   
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.006        0.006     
      API        0.002        0.001     
      Black      0.006        0.049     
      Hispanic   0.004        0.004     
      White      0.960        0.893     
      Other      0.021        0.047     
      ---------------------------------------- 
      
      Surname:  Smith     
      State:    WA        
      County:   King      
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.013        0.008     
      API        0.007        0.020     
      Black      0.139        0.145     
      Hispanic   0.015        0.016     
      White      0.803        0.771     
      Other      0.023        0.041     
      ---------------------------------------- 
      

---

    Code
      print(out)
    Output
      Surname:  Lopez     
      State:    VT        
      County:   Chittenden
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.004        0.004     
      API        0.060        0.032     
      Black      0.001        0.006     
      Hispanic   0.626        0.626     
      White      0.304        0.319     
      Other      0.005        0.012     
      ---------------------------------------- 
      
      Only the first 1 of 3 rows printed.
      Use `print(max_print = ...)` or `options(calibisg.max_print = ...)` to print more rows.

---

    Code
      print(out, max_print = 2, digits = 5)
    Output
      Surname:  Lopez     
      State:    VT        
      County:   Chittenden
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.00444      0.00433   
      API        0.06014      0.03219   
      Black      0.00057      0.00598   
      Hispanic   0.62641      0.62633   
      White      0.30382      0.31905   
      Other      0.00462      0.01213   
      ---------------------------------------- 
      
      Surname:  Jackson   
      State:    VT        
      County:   Windsor   
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0.00650      0.00554   
      API        0.00198      0.00087   
      Black      0.00595      0.04893   
      Hispanic   0.00416      0.00405   
      White      0.96004      0.89317   
      Other      0.02138      0.04744   
      ---------------------------------------- 
      
      Only the first 2 of 3 rows printed.
      Use `print(max_print = ...)` or `options(calibisg.max_print = ...)` to print more rows.

# print.compare_calibisg() handles edge cases

    Code
      print(out, max_print = 0)
    Output
      Only the first 0 of 1 rows printed.
      Use `print(max_print = ...)` or `options(calibisg.max_print = ...)` to print more rows.

---

    Code
      print(out, digits = 0)
    Output
      Surname:  Smith     
      State:    WA        
      County:   King      
      Year:     2020      
      
      Race       Pr_calibisg  Pr_bisg   
      ---------------------------------------- 
      AIAN       0            0         
      API        0            0         
      Black      0            0         
      Hispanic   0            0         
      White      1            1         
      Other      0            0         
      ---------------------------------------- 
      

# most_probable_race() output hasn't changed

    structure(list(name = c("lopez", "jackson", "smith", "chan", 
    "noname", "thomas"), year = c(2020, 2020, 2020, 2020, 2020, 2020
    ), state = c("VT", "OK", "WA", "NC", "WA", "OK"), county = c("chittenden", 
    "tulsa", "king", "wake", "king", "noname"), calibisg_race = c("hispanic", 
    "white_nh", "white_nh", "api", NA, NA), bisg_race = c("hispanic", 
    "black_nh", "white_nh", "api", "white_nh", NA), in_census = c(TRUE, 
    TRUE, TRUE, TRUE, NA, NA)), class = "data.frame", row.names = c(NA, 
    6L))

