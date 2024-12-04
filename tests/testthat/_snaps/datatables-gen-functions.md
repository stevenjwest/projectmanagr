# datatable_create()

    Code
      cat(datatable_create(IDs, data_cols, datatable_name, default_data_vals,
        dt_length, expand), sep = "\n")
    Output
      
      projectmanagr::datatable_create():
      
        add IDs
      
      +===============================================================================
      
      
          samples  :  CREATE
      
      
              ID      
          ==========  
      
           DATA_VAL   
                      
      
      +===============================================================================
      

---

    Code
      cat(datatable_create(IDs, data_cols, datatable_name, default_data_vals,
        dt_length, expand), sep = "\n")
    Output
      
      projectmanagr::datatable_create():
      
        add IDs
          add col:  c
        data col:: c  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  cage
        data col:: cage  index:  2
          data_col_wds: 9
          col_spacers_len: 12
          add col:  genotype
        data col:: genotype  index:  3
          data_col_wds: 12
          col_spacers_len: 12
          add col:  strain_breed_type
        data col:: strain_breed_type  index:  4
          data_col_wds: 21
          col_spacers_len: 12
          add col:  dob_dt
        data col:: dob_dt  index:  5
          data_col_wds: 20
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  CREATE
      
      
            ID       c        cage      genotype      strain_breed_type           dob_dt         
          ======  =======  =========  ============  =====================  ====================  
      
           1001     VAL      VALUE      DATA_VAL         DATA__VALUE         INSERT__DATETIME    
                                                                                                 
           1002     VAL      VALUE      DATA_VAL         DATA__VALUE         INSERT__DATETIME    
                                                                                                 
           1003     VAL      VALUE      DATA_VAL         DATA__VALUE         INSERT__DATETIME    
                                                                                                 
           1004     VAL      VALUE      DATA_VAL         DATA__VALUE         INSERT__DATETIME    
                                                                                                 
      
      +===============================================================================
      

---

    Code
      cat(datatable_create(IDs, data_cols, datatable_name, default_data_vals,
        dt_length, expand), sep = "\n")
    Output
      
      projectmanagr::datatable_create():
      
        add IDs
          add col:  c
        data col:: c  index:  1
          data_col_wds: 5
          col_spacers_len: 12
          add col:  cage
        data col:: cage  index:  2
          data_col_wds: 10
          col_spacers_len: 12
          add col:  genotype
        data col:: genotype  index:  3
          data_col_wds: 12
          col_spacers_len: 12
          add col:  strain_breed_type
        data col:: strain_breed_type  index:  4
          data_col_wds: 21
          col_spacers_len: 12
          add col:  dob_dt
        data col:: dob_dt  index:  5
          data_col_wds: 20
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  CREATE
      
      
            ID      c       cage       genotype      strain_breed_type           dob_dt         
          ======  =====  ==========  ============  =====================  ====================  
      
           1001     F      CID101       vgat:wt            c57bl1           2024-08-21:12:11    
                                                                                                
           1002     F      CID102       vgat:wt            c57bl2           2024-08-21:12:12    
                                                                                                
           1003     M      CID103       vgat:wt            c57bl3           2024-08-21:12:13    
                                                                                                
           1004     M      CID104       vgat:wt            c57bl4           2024-08-21:12:14    
                                                                                                
      
      +===============================================================================
      

---

    Code
      cat(datatable_create(IDs, data_cols, datatable_name, default_data_vals,
        dt_length, expand), sep = "\n")
    Output
      
      projectmanagr::datatable_create():
      
        add IDs
        add IDs
          add col:  x
        data col:: x  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  wt-g
        data col:: wt-g  index:  2
          data_col_wds: 9
          col_spacers_len: 12
          add col:  perfuse_dt
        data col:: perfuse_dt  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfusion_con
        data col:: perfusion_con  index:  4
          data_col_wds: 17
          col_spacers_len: 12
          add col:  group-fix
        data col:: group-fix  index:  5
          data_col_wds: 15
          col_spacers_len: 12
          add col:  postfix_dt
        data col:: postfix_dt  index:  6
          data_col_wds: 20
          col_spacers_len: 12
          add col:  postfix_con
        data col:: postfix_con  index:  7
          data_col_wds: 15
          col_spacers_len: 12
          add col:  group-postfix
        data col:: group-postfix  index:  8
          data_col_wds: 17
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  CREATE
      
      
            ID       x        wt-g         perfuse_dt         perfusion_con       group-fix     
          ======  =======  =========  ====================  =================  ===============  
      
           1001     VAL      VALUE      INSERT__DATETIME       DATA__VALUE       DATA__VALUE    
                                                                                                
           1002     VAL      VALUE      INSERT__DATETIME       DATA__VALUE       DATA__VALUE    
                                                                                                
           1003     VAL      VALUE      INSERT__DATETIME       DATA__VALUE       DATA__VALUE    
                                                                                                
           1004     VAL      VALUE      INSERT__DATETIME       DATA__VALUE       DATA__VALUE    
                                                                                                
      
      +===============================================================================
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID         postfix_dt         postfix_con      group-postfix    
          ======  ====================  ===============  =================  
      
           1001     INSERT__DATETIME      DATA__VALUE       DATA__VALUE     
                                                                            
           1002     INSERT__DATETIME      DATA__VALUE       DATA__VALUE     
                                                                            
           1003     INSERT__DATETIME      DATA__VALUE       DATA__VALUE     
                                                                            
           1004     INSERT__DATETIME      DATA__VALUE       DATA__VALUE     
                                                                            
      
      +===============================================================================
      

---

    Code
      cat(datatable_create(IDs, data_cols, datatable_name, default_data_vals,
        dt_length, expand), sep = "\n")
    Output
      
      projectmanagr::datatable_create():
      
        add IDs
          add col:  c
        data col:: c  index:  1
          data_col_wds: 5
          col_spacers_len: 12
          add col:  cage
        data col:: cage  index:  2
          data_col_wds: 10
          col_spacers_len: 12
          add col:  genotype
        data col:: genotype  index:  3
          data_col_wds: 12
          col_spacers_len: 12
          add col:  strain_breed_type
        data col:: strain_breed_type  index:  4
          data_col_wds: 21
          col_spacers_len: 12
          add col:  dob_dt
        data col:: dob_dt  index:  5
          data_col_wds: 20
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  CREATE
      
      
            ID      c       cage       genotype      strain_breed_type           dob_dt         
          ======  =====  ==========  ============  =====================  ====================  
      
           1001     M      CID101       vgat:wt            c57bl6           2024-08-21:12:17    
                                                                                                
           1002     M      CID101       vgat:wt            c57bl6           2024-08-21:12:17    
                                                                                                
           1003     M      CID101       vgat:wt            c57bl6           2024-08-21:12:17    
                                                                                                
           1004     M      CID101       vgat:wt            c57bl6           2024-08-21:12:17    
                                                                                                
      
      +===============================================================================
      

# datatable_add_data_samples()

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  3
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  4
          data_col_wds: 9
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID       perfuse_wash_dt      perfuse_wash_con       p        perf    
          ======  ====================  ====================  =======  =========  
      
           1001     INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                  
           1002     INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                  
           1003     INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                  
           1004     INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  3
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  4
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  ====================  ====================  =======  ========  
      
           1001     2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
           1002     2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
           1003     2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
           1004     2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  3
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  4
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  ====================  ====================  =======  ========  
      
           1001     2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
           1002     2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
           1003     2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
           1004     2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 11
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  2
          data_col_wds: 20
          col_spacers_len: 11
          add col:  p
        data col:: p  index:  3
          data_col_wds: 7
          col_spacers_len: 11
          add col:  perf
        data col:: perf  index:  4
          data_col_wds: 8
          col_spacers_len: 11
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID      perfuse_wash_dt      perfuse_wash_con       p       perf    
          =====  ====================  ====================  =======  ========  
      
           ALL     2024-08-19:14:12           PBS_RT           PBS       RT     
                   2024-08-19:14:12           PBS_RT           PBS       RT     
                   2024-08-19:14:12           PBS_RT           PBS       RT     
                   2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 14
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  2
          data_col_wds: 20
          col_spacers_len: 14
          add col:  p
        data col:: p  index:  3
          data_col_wds: 7
          col_spacers_len: 14
          add col:  perf
        data col:: perf  index:  4
          data_col_wds: 8
          col_spacers_len: 14
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
             ID        perfuse_wash_dt      perfuse_wash_con       p       perf    
          ========  ====================  ====================  =======  ========  
      
           fix_4C     2024-08-19:14:12           PBS_RT           PBS       RT     
                      2024-08-19:14:12           PBS_RT           PBS       RT     
                      2024-08-19:14:12           PBS_RT           PBS       RT     
                      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                   
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  3
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  4
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  ====================  ====================  =======  ========  
      
           1001     2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
           1003     2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                    2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                 
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  fix_dt
        data col:: fix_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  2
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID           fix_dt            fix_con    
          ======  ====================  ============  
      
           1003     INSERT__DATETIME      DATA_VAL    
                                                      
           1004     INSERT__DATETIME      DATA_VAL    
                                                      
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  fix_dt
        data col:: fix_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  2
          data_col_wds: 11
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID           fix_dt           fix_con    
          ======  ====================  ===========  
      
           1003     2024-08-19:1200B       PBS_RT    
                                                     
           1004     2024-08-19:1200B       PBS_RT    
                                                     
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  fix_dt
        data col:: fix_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  2
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID           fix_dt            fix_con    
          ======  ====================  ============  
      
           1003     INSERT__DATETIME      DATA_VAL    
                                                      
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  fix_dt
        data col:: fix_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  2
          data_col_wds: 11
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
            ID           fix_dt           fix_con    
          ======  ====================  ===========  
      
           1003     2024-08-19:1200B       PBS_RT    
                                                     
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          ======  =======  ====================  ====================  =======  =========  
      
           1001      1       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      2       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      3       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      4       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      5       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      6       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      7       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      8       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002      1       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002      2       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002      3       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002      4       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002      5       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002      6       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002      7       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002      8       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      1       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      2       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      3       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      4       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      5       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      6       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      7       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      8       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004      1       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004      2       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004      3       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004      4       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004      5       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004      6       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004      7       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004      8       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          ======  =======  ====================  ====================  =======  =========  
      
           1001     1:8      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002     1:8      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003     1:8      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004     1:8      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          ======  =======  ====================  ====================  =======  =========  
      
           1001     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1002     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1004     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  =======  ====================  ====================  =======  ========  
      
           1001      1       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      2       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      3       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      4       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      5       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      6       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      7       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      8       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      1       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      2       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      3       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      4       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      5       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      6       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      7       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      8       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      1       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      2       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      3       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      4       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      5       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      6       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      7       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      8       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      1       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      2       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      3       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      4       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      5       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      6       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      7       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      8       2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  =======  ====================  ====================  =======  ========  
      
           1001     1:8      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002     1:8      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003     1:8      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004     1:8      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  =======  ====================  ====================  =======  ========  
      
           1001     ALL      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002     ALL      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003     ALL      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004     ALL      2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  =======  ====================  ====================  =======  ========  
      
           1001      1       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      2       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      3       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      4       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      5       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      6       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      7       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1001      8       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      1       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      2       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      3       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      4       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      5       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      6       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      7       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002      8       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      1       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      2       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      3       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      4       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      5       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      6       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      7       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003      8       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      1       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      2       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      3       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      4       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      5       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      6       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      7       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004      8       2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  =======  ====================  ====================  =======  ========  
      
           1001     1:8      2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002     1:8      2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003     1:8      2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004     1:8      2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p       perf    
          ======  =======  ====================  ====================  =======  ========  
      
           1001     ALL      2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1002     ALL      2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1003     ALL      2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
           1004     ALL      2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                             2024-08-19:14:12           PBS_RT           PBS       RT     
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 11
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 11
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 11
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 11
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 11
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID     rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          =====  =======  ====================  ====================  =======  =========  
      
           ALL     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 11
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 11
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 11
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 11
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 11
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID     rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          =====  =======  ====================  ====================  =======  =========  
      
           ALL     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 11
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 11
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 11
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 11
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 11
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID     rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          =====  =======  ====================  ====================  =======  =========  
      
           ALL     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          ======  =======  ====================  ====================  =======  =========  
      
           1001      1       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      2       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      3       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      4       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      5       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      6       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      7       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1001      8       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      1       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      2       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      3       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      4       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      5       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      6       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      7       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003      8       INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          ======  =======  ====================  ====================  =======  =========  
      
           1001     1:8      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003     1:8      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  p
        data col:: p  index:  4
          data_col_wds: 7
          col_spacers_len: 12
          add col:  perf
        data col:: perf  index:  5
          data_col_wds: 9
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep       perfuse_wash_dt      perfuse_wash_con       p        perf    
          ======  =======  ====================  ====================  =======  =========  
      
           1001     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
           1003     ALL      INSERT__DATETIME         DATA__VALUE        VAL      VALUE    
                                                                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt            fix_con    
          ======  =======  ====================  ============  
      
           1003      1       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      2       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      3       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      4       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      5       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      6       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      7       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      8       INSERT__DATETIME      DATA_VAL    
                                                               
           1004      1       INSERT__DATETIME      DATA_VAL    
                                                               
           1004      2       INSERT__DATETIME      DATA_VAL    
                                                               
           1004      3       INSERT__DATETIME      DATA_VAL    
                                                               
           1004      4       INSERT__DATETIME      DATA_VAL    
                                                               
           1004      5       INSERT__DATETIME      DATA_VAL    
                                                               
           1004      6       INSERT__DATETIME      DATA_VAL    
                                                               
           1004      7       INSERT__DATETIME      DATA_VAL    
                                                               
           1004      8       INSERT__DATETIME      DATA_VAL    
                                                               
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt            fix_con    
          ======  =======  ====================  ============  
      
           1003     1:8      INSERT__DATETIME      DATA_VAL    
                                                               
           1004     1:8      INSERT__DATETIME      DATA_VAL    
                                                               
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt            fix_con    
          ======  =======  ====================  ============  
      
           1003     ALL      INSERT__DATETIME      DATA_VAL    
                                                               
           1004     ALL      INSERT__DATETIME      DATA_VAL    
                                                               
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 11
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt           fix_con    
          ======  =======  ====================  ===========  
      
           1003      1       2024-08-19:1200B       PBS_RT    
                                                              
           1003      2       2024-08-19:1200B       PBS_RT    
                                                              
           1003      3       2024-08-19:1200B       PBS_RT    
                                                              
           1003      4       2024-08-19:1200B       PBS_RT    
                                                              
           1003      5       2024-08-19:1200B       PBS_RT    
                                                              
           1003      6       2024-08-19:1200B       PBS_RT    
                                                              
           1003      7       2024-08-19:1200B       PBS_RT    
                                                              
           1003      8       2024-08-19:1200B       PBS_RT    
                                                              
           1004      1       2024-08-19:1200B       PBS_RT    
                                                              
           1004      2       2024-08-19:1200B       PBS_RT    
                                                              
           1004      3       2024-08-19:1200B       PBS_RT    
                                                              
           1004      4       2024-08-19:1200B       PBS_RT    
                                                              
           1004      5       2024-08-19:1200B       PBS_RT    
                                                              
           1004      6       2024-08-19:1200B       PBS_RT    
                                                              
           1004      7       2024-08-19:1200B       PBS_RT    
                                                              
           1004      8       2024-08-19:1200B       PBS_RT    
                                                              
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 11
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt           fix_con    
          ======  =======  ====================  ===========  
      
           1003     1:8      2024-08-19:1200B       PBS_RT    
                                                              
           1004     1:8      2024-08-19:1200B       PBS_RT    
                                                              
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 11
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt           fix_con    
          ======  =======  ====================  ===========  
      
           1003     ALL      2024-08-19:1200B       PBS_RT    
                                                              
           1004     ALL      2024-08-19:1200B       PBS_RT    
                                                              
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt            fix_con    
          ======  =======  ====================  ============  
      
           1003      1       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      2       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      3       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      4       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      5       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      6       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      7       INSERT__DATETIME      DATA_VAL    
                                                               
           1003      8       INSERT__DATETIME      DATA_VAL    
                                                               
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 11
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt           fix_con    
          ======  =======  ====================  ===========  
      
           1003      1       2024-08-19:1200B       PBS_RT    
                                                              
           1003      2       2024-08-19:1200B       PBS_RT    
                                                              
           1003      3       2024-08-19:1200B       PBS_RT    
                                                              
           1003      4       2024-08-19:1200B       PBS_RT    
                                                              
           1003      5       2024-08-19:1200B       PBS_RT    
                                                              
           1003      6       2024-08-19:1200B       PBS_RT    
                                                              
           1003      7       2024-08-19:1200B       PBS_RT    
                                                              
           1003      8       2024-08-19:1200B       PBS_RT    
                                                              
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt            fix_con    
          ======  =======  ====================  ============  
      
           1003     1:8      INSERT__DATETIME      DATA_VAL    
                                                               
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt            fix_con    
          ======  =======  ====================  ============  
      
           1003     ALL      INSERT__DATETIME      DATA_VAL    
                                                               
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 11
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt           fix_con    
          ======  =======  ====================  ===========  
      
           1003     1:8      2024-08-19:1200B       PBS_RT    
                                                              
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  fix_dt
        data col:: fix_dt  index:  2
          data_col_wds: 20
          col_spacers_len: 12
          add col:  fix_con
        data col:: fix_con  index:  3
          data_col_wds: 11
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      rep           fix_dt           fix_con    
          ======  =======  ====================  ===========  
      
           1003     ALL      2024-08-19:1200B       PBS_RT    
                                                              
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_samples(contents, data_cols, datatable_name, ids_vector,
        default_data_vals, dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_samples():
      
        add IDs
          add col:  perfuse_wash_dt
        data col:: perfuse_wash_dt  index:  1
          data_col_wds: 20
          col_spacers_len: 11
          add col:  perfuse_wash_con
        data col:: perfuse_wash_con  index:  2
          data_col_wds: 20
          col_spacers_len: 11
          add col:  perf_loc
        data col:: perf_loc  index:  3
          data_col_wds: 12
          col_spacers_len: 11
          add col:  perf
        data col:: perf  index:  4
          data_col_wds: 9
          col_spacers_len: 11
          add col:  p
        data col:: p  index:  5
          data_col_wds: 7
          col_spacers_len: 11
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  ADD_DATA
      
      
            ID      perfuse_wash_dt      perfuse_wash_con      perf_loc       perf       p     
          =====  ====================  ====================  ============  =========  =======  
      
           4Hr     INSERT__DATETIME         DATA__VALUE        DATA_VAL      VALUE      VAL    
                                                                                               
      
      +===============================================================================
      

# datatable_add_data_variables()

    Code
      cat(datatable_add_data_variables(contents, var_names, datatable_name,
        group_names, default_data_vals, dt_length), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_variables():
        group_names is 'ALL'
      
        add IDs
          add col:  ALL
        data col:: ALL  index:  1
          data_col_wds: 20
          col_spacers_len: 24
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
               variables               ALL          
          ==================  ====================  
      
            perfuse_wash_dt     INSERT__DATETIME    
                                                    
           perfuse_wash_con        DATA__VALUE      
                                                    
                   p                   VAL          
                                                    
                 perf                 VALUE         
                                                    
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_variables(contents, var_names, datatable_name,
        group_names, default_data_vals, dt_length), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_variables():
        group_names is a group:  group-fix 
      
        add IDs
          add col:  fix_RT
        data col:: fix_RT  index:  1
          data_col_wds: 20
          col_spacers_len: 24
          add col:  fix_4C
        data col:: fix_4C  index:  2
          data_col_wds: 20
          col_spacers_len: 24
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
               variables             fix_RT                fix_4C         
          ==================  ====================  ====================  
      
            perfuse_wash_dt     INSERT__DATETIME      INSERT__DATETIME    
                                                                          
           perfuse_wash_con        DATA__VALUE           DATA__VALUE      
                                                                          
                   p                   VAL                   VAL          
                                                                          
                 perf                 VALUE                 VALUE         
                                                                          
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_variables(contents, var_names, datatable_name,
        group_names, default_data_vals, dt_length), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_variables():
        group_names is a group:  group-fix 
      
        add IDs
          add col:  fix_RT
        data col:: fix_RT  index:  1
          data_col_wds: 15
          col_spacers_len: 24
          add col:  fix_4C
        data col:: fix_4C  index:  2
          data_col_wds: 15
          col_spacers_len: 24
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
               variables           fix_RT           fix_4C      
          ==================  ===============  ===============  
      
            perfuse_wash_dt     RT_Datetime      RT_Datetime    
                                                                
           perfuse_wash_con     RT_wash_con      RT_wash_con    
                                                                
                   p                RT_p             RT_p       
                                                                
                 perf             RT_perf          RT_perf      
                                                                
      
      +===============================================================================
      

# datatable_add_data_timetable()

    Code
      cat(datatable_add_data_timetable(contents, step_names, datatable_name,
        group_names, col_name, dt_length), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_timetable():
        group_names is a group:  group-fix 
      
        add IDs
          add col:  fix_4C
        data col:: fix_4C  index:  1
          data_col_wds: 10
          col_spacers_len: 17
          add col:  fix_RT
        data col:: fix_RT  index:  2
          data_col_wds: 10
          col_spacers_len: 17
          add col:  delip_dt
        data col:: delip_dt  index:  3
          data_col_wds: 20
          col_spacers_len: 17
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
           timetable     fix_4C      fix_RT          delip_dt        
          ===========  ==========  ==========  ====================  
      
              0:00        HT_RT       HT_RT      INSERT__DATETIME    
                                                                     
              0:10        MT_RT       MT_RT      INSERT__DATETIME    
                                                                     
              0:20       DMT_RT      DMT_RT      INSERT__DATETIME    
                                                                     
              0:30        MT_RT       MT_RT      INSERT__DATETIME    
                                                                     
              0:40        HT_RT       HT_RT      INSERT__DATETIME    
                                                                     
              0:50       PBS_RT      PBS_RT      INSERT__DATETIME    
                                                                     
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_data_timetable(contents, step_names, datatable_name,
        group_names, col_name, dt_length), sep = "\n")
    Output
      
      projectmanagr::datatable_add_data_timetable():
        group_names is a group:  group-fix 
      
        add IDs
          add col:  fix_RT
        data col:: fix_RT  index:  1
          data_col_wds: 10
          col_spacers_len: 17
          add col:  fix_4C
        data col:: fix_4C  index:  2
          data_col_wds: 10
          col_spacers_len: 17
          add col:  delip_dt
        data col:: delip_dt  index:  3
          data_col_wds: 20
          col_spacers_len: 17
      
      +===============================================================================
      
      
          samples  :  ADD_DATA
      
      
           timetable     fix_RT      fix_4C          delip_dt        
          ===========  ==========  ==========  ====================  
      
              0:00        HT_RT       HT_RT      INSERT__DATETIME    
                                                                     
              0:10        MT_RT       MT_RT      INSERT__DATETIME    
                                                                     
              0:20       DMT_RT      DMT_RT      INSERT__DATETIME    
                                                                     
              0:30        MT_RT       MT_RT      INSERT__DATETIME    
                                                                     
              0:40        HT_RT       HT_RT      INSERT__DATETIME    
                                                                     
              0:50       PBS_RT      PBS_RT      INSERT__DATETIME    
                                                                     
      
      +===============================================================================
      

# datatable_add_group()

    Code
      cat(datatable_add_group(contents, group_names, datatable_name, groups,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_groups():
      
        add IDs
          add col:  group-solvent-inc
        data col:: group-solvent-inc  index:  1
          data_col_wds: 21
          col_spacers_len: 12
          add col:  group-ab-conc
        data col:: group-ab-conc  index:  2
          data_col_wds: 17
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  GROUP
      
      
            ID      group-solvent-inc      group-ab-conc    
          ======  =====================  =================  
      
           1001            1Hr                 1mg/mL       
                                                            
           1002            2Hr                0.5mg/mL      
                                                            
           1003            4Hr                 1mg/mL       
                                                            
           1004            1Hr                0.5mg/mL      
                                                            
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_group(contents, group_names, datatable_name, groups,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_groups():
      
        add IDs
          add col:  group_solvent_inc
        data col:: group_solvent_inc  index:  1
          data_col_wds: 21
          col_spacers_len: 12
          add col:  group_ab
        data col:: group_ab  index:  2
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  GROUP
      
      
            ID      group_solvent_inc      group_ab    
          ======  =====================  ============  
      
           1001        DATA__VALUE         DATA_VAL    
                                                       
           1002        DATA__VALUE         DATA_VAL    
                                                       
           1003        DATA__VALUE         DATA_VAL    
                                                       
           1004        DATA__VALUE         DATA_VAL    
                                                       
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_group(contents, group_names, datatable_name, groups,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_groups():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  group-solvent-inc
        data col:: group-solvent-inc  index:  2
          data_col_wds: 21
          col_spacers_len: 12
          add col:  group-ab-conc
        data col:: group-ab-conc  index:  3
          data_col_wds: 17
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  GROUP
      
      
            ID      rep      group-solvent-inc      group-ab-conc    
          ======  =======  =====================  =================  
      
           1001      1              1Hr                 1mg/mL       
                                                                     
           1001      2              2Hr                0.5mg/mL      
                                                                     
           1002      1              4Hr                 1mg/mL       
                                                                     
           1002      2              1Hr                0.5mg/mL      
                                                                     
           1003      1              2Hr                 1mg/mL       
                                                                     
           1003      2              4Hr                0.5mg/mL      
                                                                     
           1004      1              1Hr                 1mg/mL       
                                                                     
           1004      2              2Hr                0.5mg/mL      
                                                                     
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_group(contents, group_names, datatable_name, groups,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_groups():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  group-solvent-inc
        data col:: group-solvent-inc  index:  2
          data_col_wds: 21
          col_spacers_len: 12
          add col:  group-ab-conc
        data col:: group-ab-conc  index:  3
          data_col_wds: 17
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  GROUP
      
      
            ID      rep      group-solvent-inc      group-ab-conc    
          ======  =======  =====================  =================  
      
           1001     1:2             1Hr                 1mg/mL       
                                                                     
           1002     1:2             2Hr                0.5mg/mL      
                                                                     
           1003     1:2             4Hr                 1mg/mL       
                                                                     
           1004     1:2             1Hr                0.5mg/mL      
                                                                     
      
      +===============================================================================
      

---

    Code
      cat(datatable_add_group(contents, group_names, datatable_name, groups,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_add_groups():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  group-solvent-inc
        data col:: group-solvent-inc  index:  2
          data_col_wds: 21
          col_spacers_len: 12
          add col:  group-ab-conc
        data col:: group-ab-conc  index:  3
          data_col_wds: 17
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  GROUP
      
      
            ID      rep      group-solvent-inc      group-ab-conc    
          ======  =======  =====================  =================  
      
           1001     ALL             1Hr                 1mg/mL       
                                                                     
           1002     ALL             2Hr                0.5mg/mL      
                                                                     
           1003     ALL             4Hr                 1mg/mL       
                                                                     
           1004     ALL             1Hr                0.5mg/mL      
                                                                     
      
      +===============================================================================
      

# datatable_dispose()

    Code
      cat(datatable_dispose(contents, datatable_name, dt_length, summarise_reps,
        all_reps, cdt), sep = "\n")
    Output
      
      projectmanagr::datatable_dispose():
      
        add IDs
          add col:  dispose
        data col:: dispose  index:  1
          data_col_wds: 20
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  DISPOSE
      
      
            ID           dispose        
          ======  ====================  
      
           1001     2024-08-16:14:23    
                                        
           1002     2024-08-16:14:23    
                                        
           1003     2024-08-16:14:23    
                                        
           1004     2024-08-16:14:23    
                                        
      
      +===============================================================================
      

# datatable_resample()

    Code
      cat(datatable_resample(contents, datatable_name, resample_vector, rep_vector,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_resample():
      
        add IDs
          add col:  resample
        data col:: resample  index:  1
          data_col_wds: 13
          col_spacers_len: 12
          add col:  reps
        data col:: reps  index:  2
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  RESAMPLE
      
      
            ID       resample      reps    
          ======  =============  ========  
      
           1001        CNS           1     
                      SC-LUM               
                    DRG-L4-LT              
                                           
           1002        CNS           1     
                      SC-LUM               
                    DRG-L4-LT              
                                           
           1003        CNS           1     
                      SC-LUM               
                    DRG-L4-LT              
                                           
           1004        CNS           1     
                      SC-LUM               
                    DRG-L4-LT              
                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_resample(contents, datatable_name, resample_vector, rep_vector,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_resample():
      
        add IDs
          add col:  resample
        data col:: resample  index:  1
          data_col_wds: 13
          col_spacers_len: 12
          add col:  reps
        data col:: reps  index:  2
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  RESAMPLE
      
      
            ID       resample      reps    
          ======  =============  ========  
      
           1001        CNS           1     
                      SC-LUM         1     
                    DRG-L4-LT        1     
                                           
           1002        CNS           1     
                      SC-LUM         1     
                    DRG-L4-LT        1     
                                           
           1003        CNS           1     
                      SC-LUM         1     
                    DRG-L4-LT        1     
                                           
           1004        CNS           1     
                      SC-LUM         1     
                    DRG-L4-LT        1     
                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_resample(contents, datatable_name, resample_vector, rep_vector,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_resample():
      
        add IDs
          add col:  resample
        data col:: resample  index:  1
          data_col_wds: 13
          col_spacers_len: 12
          add col:  reps
        data col:: reps  index:  2
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  RESAMPLE
      
      
            ID       resample      reps    
          ======  =============  ========  
      
           1001        CNS           1     
                      SC-LUM         1     
                    DRG-L4-LT        2     
                                           
           1002        CNS           1     
                      SC-LUM         1     
                    DRG-L4-LT        2     
                                           
           1003        CNS           1     
                      SC-LUM         1     
                    DRG-L4-LT        2     
                                           
           1004        CNS           1     
                      SC-LUM         1     
                    DRG-L4-LT        2     
                                           
      
      +===============================================================================
      

---

    Code
      cat(datatable_resample(contents, datatable_name, resample_vector, rep_vector,
        dt_length, summarise_reps, all_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_resample():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  resample
        data col:: resample  index:  2
          data_col_wds: 12
          col_spacers_len: 12
          add col:  reps
        data col:: reps  index:  3
          data_col_wds: 8
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  RESAMPLE
      
      
            ID      rep      resample      reps    
          ======  =======  ============  ========  
      
           1001     1:8        DH-LT         1     
                               DH-RT               
                               VH-LT               
                               VH-RT               
                                                   
           1002     1:8        DH-LT         1     
                               DH-RT               
                               VH-LT               
                               VH-RT               
                                                   
           1003     1:8        DH-LT         1     
                               DH-RT               
                               VH-LT               
                               VH-RT               
                                                   
           1004     1:8        DH-LT         1     
                               DH-RT               
                               VH-LT               
                               VH-RT               
                                                   
      
      +===============================================================================
      

# datatable_export()

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  export
        data col:: export  index:  1
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  EXPORT
      
      
            ID                            export                         
          ======  =====================================================  
      
           1001     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
           1002     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
           1003     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
           1004     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  export
        data col:: export  index:  1
          data_col_wds: 64
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  EXPORT
      
      
            ID                                 export                               
          ======  ================================================================  
      
           1001   [dn-t___001](../../../../../programme-path/project-doc-path/proj  
                    ect-note-path/note-sub-path/dn-t/dn-t___001_--_dest_note.Rmd)   
                                                                                    
           1002   [dn-t___001](../../../../../programme-path/project-doc-path/proj  
                    ect-note-path/note-sub-path/dn-t/dn-t___001_--_dest_note.Rmd)   
                                                                                    
           1003   [dn-t___001](../../../../../programme-path/project-doc-path/proj  
                    ect-note-path/note-sub-path/dn-t/dn-t___001_--_dest_note.Rmd)   
                                                                                    
           1004   [dn-t___001](../../../../../programme-path/project-doc-path/proj  
                    ect-note-path/note-sub-path/dn-t/dn-t___001_--_dest_note.Rmd)   
                                                                                    
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  export
        data col:: export  index:  1
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  EXPORT
      
      
            ID                            export                         
          ======  =====================================================  
      
           1002     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
           1003     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  export
        data col:: export  index:  1
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  EXPORT
      
      
            ID                            export                         
          ======  =====================================================  
      
           1004     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
           1002     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
           1003     [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                         
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1001      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1001      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1002      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1002      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1003      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1003      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1004      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1004      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1001     1:2      [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1002     1:2      [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1003     1:2      [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1004     1:2      [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1002      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1002      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1002     1:2      [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1002      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1002      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1002      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1003      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1001      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1002      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1002     1:2      [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1003      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1001      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm-1_DH-RT  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1002      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1003      3       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1004      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1001      3       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1003      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

---

    Code
      cat(datatable_export(contents, datatable_name, destination_rel_path_link,
        ids_vector, reps_vector, dt_length, summarise_reps), sep = "\n")
    Output
      
      projectmanagr::datatable_export():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  export
        data col:: export  index:  2
          data_col_wds: 53
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm-1_DH-RT  :  EXPORT
      
      
            ID      rep                            export                         
          ======  =======  =====================================================  
      
           1002      1       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1003     1,3      [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1004      2       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
           1001      3       [dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)    
                                                                                  
      
      +===============================================================================
      

# datatable_import()

    Code
      cat(datatable_import(source_contents, destination_contents, datatable_name,
        source_rel_path_link, ids_vector, reps_vector, dt_length, summarise_reps),
      sep = "\n")
    Output
      
      projectmanagr::datatable_import():
      
        add IDs
          add col:  import
        data col:: import  index:  1
          data_col_wds: 61
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  IMPORT
      
      
            ID                                import                             
          ======  =============================================================  
      
           1001     [sn-t___001](../sn-t/sn-t___001_--_source_note.Rmd)::1001    
                                                                                 
           1002     [sn-t___001](../sn-t/sn-t___001_--_source_note.Rmd)::1002    
                                                                                 
           1003     [sn-t___001](../sn-t/sn-t___001_--_source_note.Rmd)::1003    
                                                                                 
           1004     [sn-t___001](../sn-t/sn-t___001_--_source_note.Rmd)::1004    
                                                                                 
      
      +===============================================================================
      

---

    Code
      cat(datatable_import(source_contents, destination_contents, datatable_name,
        source_rel_path_link, ids_vector, reps_vector, dt_length, summarise_reps),
      sep = "\n")
    Output
      
      projectmanagr::datatable_import():
      
        add IDs
          add col:  import
        data col:: import  index:  1
          data_col_wds: 69
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  IMPORT
      
      
            ID                                    import                                 
          ======  =====================================================================  
      
           1001   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1001    
                                                                                         
           1002   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002    
                                                                                         
           1003   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1003    
                                                                                         
           1004   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1004    
                                                                                         
      
      +===============================================================================
      

---

    Code
      cat(datatable_import(source_contents, destination_contents, datatable_name,
        source_rel_path_link, ids_vector, reps_vector, dt_length, summarise_reps),
      sep = "\n")
    Output
      
      projectmanagr::datatable_import():
      
        add IDs
          add col:  import
        data col:: import  index:  1
          data_col_wds: 69
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  IMPORT
      
      
            ID                                    import                                 
          ======  =====================================================================  
      
           1002   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002    
                                                                                         
           1004   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1004    
                                                                                         
      
      +===============================================================================
      

---

    Code
      cat(datatable_import(source_contents, destination_contents, datatable_name,
        source_rel_path_link, ids_vector, reps_vector, dt_length, summarise_reps),
      sep = "\n")
    Output
      
      projectmanagr::datatable_import():
      
        add IDs
          add col:  import
        data col:: import  index:  1
          data_col_wds: 69
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  IMPORT
      
      
            ID                                    import                                 
          ======  =====================================================================  
      
           1003   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1003    
                                                                                         
           1002   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002    
                                                                                         
           1004   [sn-t___001]((../../../../../programme-path/project-doc-path/project-  
                    note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1004    
                                                                                         
      
      +===============================================================================
      

---

    Code
      cat(datatable_import(source_contents, destination_contents, datatable_name,
        source_rel_path_link, ids_vector, reps_vector, dt_length, summarise_reps),
      sep = "\n")
    Output
      
      projectmanagr::datatable_import():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  import
        data col:: import  index:  2
          data_col_wds: 70
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  IMPORT
      
      
            ID      rep                                    import                                  
          ======  =======  ======================================================================  
      
           1001      1     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1001::1   
                                                                                                   
           1001      2     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1001::2   
                                                                                                   
           1002      1     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002::1   
                                                                                                   
           1002      2     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002::2   
                                                                                                   
           1003      1     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1003::1   
                                                                                                   
           1003      2     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1003::2   
                                                                                                   
           1004      1     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1004::1   
                                                                                                   
           1004      2     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1004::2   
                                                                                                   
      
      +===============================================================================
      

---

    Code
      cat(datatable_import(source_contents, destination_contents, datatable_name,
        source_rel_path_link, ids_vector, reps_vector, dt_length, summarise_reps),
      sep = "\n")
    Output
      
      projectmanagr::datatable_import():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  import
        data col:: import  index:  2
          data_col_wds: 71
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  IMPORT
      
      
            ID      rep                                     import                                  
          ======  =======  =======================================================================  
      
           1001     1:2    [sn-t___001]((../../../../../programme-path/project-doc-path/project-no  
                             te-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1001::1:2   
                                                                                                    
           1002     1:2    [sn-t___001]((../../../../../programme-path/project-doc-path/project-no  
                             te-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002::1:2   
                                                                                                    
           1003     1:2    [sn-t___001]((../../../../../programme-path/project-doc-path/project-no  
                             te-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1003::1:2   
                                                                                                    
           1004     1:2    [sn-t___001]((../../../../../programme-path/project-doc-path/project-no  
                             te-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1004::1:2   
                                                                                                    
      
      +===============================================================================
      

---

    Code
      cat(datatable_import(source_contents, destination_contents, datatable_name,
        source_rel_path_link, ids_vector, reps_vector, dt_length, summarise_reps),
      sep = "\n")
    Output
      
      projectmanagr::datatable_import():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  import
        data col:: import  index:  2
          data_col_wds: 70
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  IMPORT
      
      
            ID      rep                                    import                                  
          ======  =======  ======================================================================  
      
           1004      2     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1004::2   
                                                                                                   
           1002      1     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002::1   
                                                                                                   
           1002      2     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002::2   
                                                                                                   
           1003      1     [sn-t___001]((../../../../../programme-path/project-doc-path/project-n  
                             ote-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1003::1   
                                                                                                   
      
      +===============================================================================
      

---

    Code
      cat(datatable_import(source_contents, destination_contents, datatable_name,
        source_rel_path_link, ids_vector, reps_vector, dt_length, summarise_reps),
      sep = "\n")
    Output
      
      projectmanagr::datatable_import():
      
        add IDs
          add col:  rep
        data col:: rep  index:  1
          data_col_wds: 7
          col_spacers_len: 12
          add col:  import
        data col:: import  index:  2
          data_col_wds: 71
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples_SC-LUM_50µm  :  IMPORT
      
      
            ID      rep                                     import                                  
          ======  =======  =======================================================================  
      
           1004      2     [sn-t___001]((../../../../../programme-path/project-doc-path/project-no  
                              te-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1004::2    
                                                                                                    
           1002     1:2    [sn-t___001]((../../../../../programme-path/project-doc-path/project-no  
                             te-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1002::1:2   
                                                                                                    
           1003      1     [sn-t___001]((../../../../../programme-path/project-doc-path/project-no  
                              te-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)::1003::1    
                                                                                                    
      
      +===============================================================================
      

# modify_matching_import_ids()

    Code
      cat(modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2,
        source_rel_path2))
    Output
      1001 3001 5001

---

    Code
      cat(modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2,
        source_rel_path2))
    Output
      1001 3001 5001

---

    Code
      cat(modify_matching_import_ids(dest_datatables = dest_datatables2,
        datatable_name = datatable_name2, dc_list = dc_list2, source_rel_path_link = source_rel_path_link2))
    Output
      1001.tnj 3001.tnj 5001

---

    Code
      cat(modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2,
        source_rel_path2))
    Output
      1001.nj0 3001.nj0 5001

---

    Code
      cat(modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2,
        source_rel_path_link2))
    Output
      1001.tnj0 3001.tnj0 5001

# datatable_build_from_tibble()

    Code
      cat(build_datatable_from_tibble(tb, datatable_name, dt_function, dt_length,
        DATATABLE_SPACER_CHAR), sep = "\n")
    Output
      
      projectmanagr::build_datatable_from_tibble():
      
        add IDs
          add col:  wash_con
        data col:: wash_con  index:  1
          data_col_wds: 12
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  CREATE
      
      
            ID      wash_con    
          ======  ============  
      
           1001      PBS_RT     
                     PBS_RT     
                                
           1002      PBS_RT     
                     PBS_RT     
                                
           1003      PBS_RT     
                     PBS_RT     
                                
           1004      PBS_RT     
                     PBS_RT     
                                
      
      +===============================================================================
      

