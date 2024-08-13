# datatable_create generates plaintext datatable

    Code
      cat(datatable_create(IDs, data_cols, datatable_name, default_data_vals,
        dt_length), sep = "\n")
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
        dt_length), sep = "\n")
    Output
      
      projectmanagr::datatable_create():
      
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
          add col:  perfusion_condition
        data col:: perfusion_condition  index:  4
          data_col_wds: 23
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  CREATE
      
      
            ID       x        wt-g         perfuse_dt         perfusion_condition    
          ======  =======  =========  ====================  =======================  
      
           1001     VAL      VALUE      INSERT__DATETIME            DATA_VAL         
                                                                                     
           1002     VAL      VALUE      INSERT__DATETIME            DATA_VAL         
                                                                                     
           1003     VAL      VALUE      INSERT__DATETIME            DATA_VAL         
                                                                                     
           1004     VAL      VALUE      INSERT__DATETIME            DATA_VAL         
                                                                                     
      
      +===============================================================================
      

---

    Code
      cat(datatable_create(IDs, data_cols, datatable_name, default_data_vals,
        dt_length), sep = "\n")
    Output
      
      projectmanagr::datatable_create():
      
        add IDs
          add col:  x
        data col:: x  index:  1
          data_col_wds: 5
          col_spacers_len: 12
          add col:  wt-g
        data col:: wt-g  index:  2
          data_col_wds: 8
          col_spacers_len: 12
          add col:  perfuse_dt
        data col:: perfuse_dt  index:  3
          data_col_wds: 20
          col_spacers_len: 12
          add col:  perfusion_condition
        data col:: perfusion_condition  index:  4
          data_col_wds: 23
          col_spacers_len: 12
      
      +===============================================================================
      
      
          samples  :  CREATE
      
      
            ID      x      wt-g         perfuse_dt         perfusion_condition    
          ======  =====  ========  ====================  =======================  
      
           1001     F      30.0      2020-01-01:12:01           F4M1PB_RT         
                                                                                  
           1002     F      31.1      2020-01-01:12:02           F4M2PB_RT         
                                                                                  
           1003     M      32.2      2020-01-01:12:03           F4M3PB_RT         
                                                                                  
           1004     M      33.3      2020-01-01:12:04           F4M4PB_RT         
                                                                                  
      
      +===============================================================================
      

