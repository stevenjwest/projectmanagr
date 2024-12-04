# datatables read rmd functions testing

    Code
      summ_dt
    Output
      # A tibble: 12 x 9
         FILENAME     ID    SAMPLE LOCATION CONDITION DATETIME COUNT IMPORT PATH      
         <chr>        <chr> <chr>  <chr>    <chr>     <chr>    <int>  <dbl> <fs::path>
       1 tn-t___001_~ 1001  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex1.Rmd
       2 tn-t___001_~ 1002  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex1.Rmd
       3 tn-t___001_~ 1003  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex1.Rmd
       4 tn-t___001_~ 1004  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex1.Rmd
       5 tn-t___002_~ 2001  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex2.Rmd
       6 tn-t___002_~ 2002  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex2.Rmd
       7 tn-t___002_~ 2003  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex2.Rmd
       8 tn-t___002_~ 2004  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex2.Rmd
       9 tn-t___003_~ 3001  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex3.Rmd
      10 tn-t___003_~ 3002  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex3.Rmd
      11 tn-t___003_~ 3003  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex3.Rmd
      12 tn-t___003_~ 3004  mice   SAAA-03~ IVC:(GM5~ 2020-03~     1      0 ~N_ex3.Rmd

---

    Code
      attr(summ_dt, "path")
    Output
      $`tn-t___001_--_PN_ex1.Rmd:::mice`
      /tmp/Rsess/_TORRF/0-PR/PD/tn-t/tn-t___001_--_PN_ex1.Rmd
      
      $`tn-t___002_--_PN_ex2.Rmd:::mice`
      /tmp/Rsess/_TORRF/0-PR/PD/tn-t/tn-t___002_--_PN_ex2.Rmd
      
      $`tn-t___003_--_PN_ex3.Rmd:::mice`
      /tmp/Rsess/_TORRF/0-PR/PD/tn-t/tn-t___003_--_PN_ex3.Rmd
      

---

    Code
      attr(summ_dt, "col_names")
    Output
      $`tn-t___001_--_PN_ex1.Rmd:::mice`
       [1] "ID"              "sex"             "dob_dt"          "colony_genotype"
       [5] "treatment"       "cage_dt"         "cage_con"        "cage_loc"       
       [9] "wts_g"           "wts_g_dt"       
      
      $`tn-t___002_--_PN_ex2.Rmd:::mice`
       [1] "ID"              "sex"             "dob_dt"          "colony_genotype"
       [5] "treatment"       "cage_dt"         "cage_con"        "cage_loc"       
       [9] "wts_g"           "wts_g_dt"       
      
      $`tn-t___003_--_PN_ex3.Rmd:::mice`
       [1] "ID"              "sex"             "dob_dt"          "colony_genotype"
       [5] "treatment"       "cage_dt"         "cage_con"        "cage_loc"       
       [9] "wts_g"           "wts_g_dt"       
      

