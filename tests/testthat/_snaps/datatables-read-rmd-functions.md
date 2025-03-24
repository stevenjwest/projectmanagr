# datatables read rmd functions testing

    Code
      summ_dt
    Output
      # A tibble: 12 x 9
         ID    SAMPLE COUNT IMPORT LOCATION     CONDITION DATETIME FILENAME PATH      
         <chr> <chr>  <int>  <dbl> <chr>        <chr>     <chr>    <chr>    <fs::path>
       1 1001  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex1.Rmd
       2 1002  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex1.Rmd
       3 1003  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex1.Rmd
       4 1004  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex1.Rmd
       5 2001  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex2.Rmd
       6 2002  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex2.Rmd
       7 2003  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex2.Rmd
       8 2004  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex2.Rmd
       9 3001  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex3.Rmd
      10 3002  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex3.Rmd
      11 3003  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex3.Rmd
      12 3004  mice       1      0 SAAA-0300102 IVC:(GM5~ 2020-03~ tn-ex__~ ~N_ex3.Rmd

---

    Code
      attr(summ_dt, "path")
    Output
      $`tn-ex___001_--_PN_ex1.Rmd:::mice`
      /var/folders/rf/4zt32l1j50q24xp1l0r7kvqc0000gn/T/Rsess/_TORRF/0-PR/PD/tn-ex/tn-ex___001_--_PN_ex1.Rmd
      
      $`tn-ex___002_--_PN_ex2.Rmd:::mice`
      /var/folders/rf/4zt32l1j50q24xp1l0r7kvqc0000gn/T/Rsess/_TORRF/0-PR/PD/tn-ex/tn-ex___002_--_PN_ex2.Rmd
      
      $`tn-ex___003_--_PN_ex3.Rmd:::mice`
      /var/folders/rf/4zt32l1j50q24xp1l0r7kvqc0000gn/T/Rsess/_TORRF/0-PR/PD/tn-ex/tn-ex___003_--_PN_ex3.Rmd
      

---

    Code
      attr(summ_dt, "col_names")
    Output
      $`tn-ex___001_--_PN_ex1.Rmd:::mice`
       [1] "ID"              "sex"             "dob_dt"          "colony_genotype"
       [5] "treatment"       "cage_dt"         "cage_con"        "cage_loc"       
       [9] "wts_g"           "wts_g_dt"       
      
      $`tn-ex___002_--_PN_ex2.Rmd:::mice`
       [1] "ID"              "sex"             "dob_dt"          "colony_genotype"
       [5] "treatment"       "cage_dt"         "cage_con"        "cage_loc"       
       [9] "wts_g"           "wts_g_dt"       
      
      $`tn-ex___003_--_PN_ex3.Rmd:::mice`
       [1] "ID"              "sex"             "dob_dt"          "colony_genotype"
       [5] "treatment"       "cage_dt"         "cage_con"        "cage_loc"       
       [9] "wts_g"           "wts_g_dt"       
      

