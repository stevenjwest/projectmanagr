# datatable_read_vector() : CREATE Datatables

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 6
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix
        <chr> <chr> <chr> <chr>            <chr>         <chr>    
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT   
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT   
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C   
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C   
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 6
        ID    x     `wt-g`    perfuse_dt                     perfusion_con `group-fix`
        <chr> <chr> <chr>     <chr>                          <chr>         <chr>      
      1 1001  F 2   30.0 30.1 2020-01-01:12:01 2020-01-01:1~ F4M1PB_RT F4~ fix_RT     
      2 1002  F 2   31.0 31.1 2020-01-02:12:01 2020-01-02:1~ F4M3PB_RT F4~ fix_RT     
      3 1003  M 2   32.0 32.1 2020-01-03:12:01 2020-01-03:1~ F4M5PB_4C F4~ fix_4C     
      4 1004  M 2   33.0 33.1 2020-01-04:12:01 2020-01-04:1~ F4M7PB_4C F4~ fix_4C     
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 8 x 6
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix
        <chr> <chr> <chr> <chr>            <chr>         <chr>    
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT   
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT   
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C   
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C   
      5 1005  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT   
      6 1006  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT   
      7 1007  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C   
      8 1008  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C   
      

---

    Code
      datatable_read_vector(contents)
    Output
      list()

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 6
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix
        <chr> <chr> <chr> <chr>            <chr>         <chr>    
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT   
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT   
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C   
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C   
      

---

    Code
      datatable_read_vector(contents)
    Output
        no datatables in contents
      list()

# datatable_read_vector() : RESAMPLE Datatables

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   1 1  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   1 1  
      
      $samples_CNS
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1001 
      2 1002 
      
      $samples_SC
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1001 
      2 1002 
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   1 1  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   1 1  
      
      $samples_CNS
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1001 
      2 1002 
      
      $samples_SC
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1001 
      2 1002 
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   3 4  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   3 4  
      
      $samples_CNS
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1002  1    
      5 1002  2    
      6 1002  3    
      
      $samples_SC
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   3 4  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   3 4  
      
      $samples_CNS
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1001  1    
      5 1001  2    
      6 1001  3    
      
      $samples_SC
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   3 1  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   3 1  
      
      $samples_CNS
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1001  1    
      5 1001  2    
      6 1001  3    
      
      $samples_SC
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1002 
      2 1001 
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample   reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>      <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC DRG 3 1 1
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC DRG 3 2 1
      
      $samples_CNS
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1001  1    
      5 1001  2    
      6 1001  3    
      
      $samples_SC
      # A tibble: 3 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1001  1    
      
      $samples_DRG
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1002 
      2 1001 
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample   reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>      <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC DRG 3 2 1
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC DRG 3 1 1
      
      $samples_CNS
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1001  1    
      5 1001  2    
      6 1001  3    
      
      $samples_SC
      # A tibble: 3 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1001  1    
      3 1001  2    
      
      $samples_DRG
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1002 
      2 1001 
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample   reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>      <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC DRG 3 2 1
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC DRG 3 1 1
      
      $samples_CNS
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1001  1    
      5 1001  2    
      6 1001  3    
      
      $samples_SC
      # A tibble: 3 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1001  1    
      3 1001  2    
      
      $samples_DRG
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1002 
      2 1001 
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   4 4  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   2 2  
      
      $samples_CNS
      # A tibble: 6 x 4
        ID    rep   resample   reps 
        <chr> <chr> <chr>      <chr>
      1 1001  1     50µm 100µm 4 4  
      2 1001  2     50µm 100µm 4 4  
      3 1001  3     50µm 100µm 4 4  
      4 1001  4     50µm 100µm 4 4  
      5 1002  1     50µm 100µm 4 4  
      6 1002  2     50µm 100µm 4 4  
      
      $samples_SC
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      
      $`samples_CNS-1_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      
      $`samples_CNS-1_100µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      
      $`samples_CNS-2_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      
      $`samples_CNS-2_100µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      
      $`samples_CNS-3_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-3_100µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_100µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   4 4  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   2 2  
      
      $samples_CNS
      # A tibble: 6 x 4
        ID    rep   resample   reps 
        <chr> <chr> <chr>      <chr>
      1 1001  1     50µm 100µm 4 4  
      2 1001  2     50µm 100µm 4 4  
      3 1001  3     50µm 100µm 4 4  
      4 1001  4     50µm 100µm 4 4  
      5 1002  1     50µm 100µm 4 4  
      6 1002  2     50µm 100µm 4 4  
      
      $samples_SC
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      
      $`samples_CNS-1_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-1_100µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-2_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-2_100µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-3_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-3_100µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_100µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   4 4  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   2 2  
      
      $samples_CNS
      # A tibble: 6 x 4
        ID    rep   resample   reps 
        <chr> <chr> <chr>      <chr>
      1 1001  1     50µm 100µm 4 4  
      2 1001  2     50µm 100µm 4 4  
      3 1001  3     50µm 100µm 4 4  
      4 1001  4     50µm 100µm 4 4  
      5 1002  1     50µm 100µm 4 4  
      6 1002  2     50µm 100µm 4 4  
      
      $samples_SC
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      
      $`samples_CNS-1_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-1_100µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-2_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-2_100µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-3_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-3_100µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_100µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   4 4  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   2 2  
      
      $samples_CNS
      # A tibble: 6 x 4
        ID    rep   resample   reps 
        <chr> <chr> <chr>      <chr>
      1 1001  1     50µm 100µm 4 4  
      2 1001  2     50µm 100µm 4 4  
      3 1001  3     50µm 100µm 4 4  
      4 1001  4     50µm 100µm 4 4  
      5 1002  1     50µm 100µm 4 1  
      6 1002  2     50µm 100µm 4 1  
      
      $samples_SC
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      
      $`samples_CNS-1_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-1_100µm`
      # A tibble: 5 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1001  1    
      3 1001  2    
      4 1001  3    
      5 1001  4    
      
      $`samples_CNS-2_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-2_100µm`
      # A tibble: 5 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1001  1    
      3 1001  2    
      4 1001  3    
      5 1001  4    
      
      $`samples_CNS-3_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-3_100µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_100µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   4 4  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   2 2  
      
      $samples_CNS
      # A tibble: 6 x 4
        ID    rep   resample         reps 
        <chr> <chr> <chr>            <chr>
      1 1001  1     50µm 100µm 250µm 4 1 1
      2 1001  2     50µm 100µm 250µm 4 1 1
      3 1001  3     50µm 100µm 250µm 4 1 1
      4 1001  4     50µm 100µm 250µm 4 1 1
      5 1002  1     50µm 100µm 250µm 4 4 1
      6 1002  2     50µm 100µm 250µm 4 4 1
      
      $samples_SC
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      
      $`samples_CNS-1_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      
      $`samples_CNS-1_100µm`
      # A tibble: 5 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1002  1    
      3 1002  2    
      4 1002  3    
      5 1002  4    
      
      $`samples_CNS-1_250µm`
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1001 
      2 1002 
      
      $`samples_CNS-2_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      
      $`samples_CNS-2_100µm`
      # A tibble: 5 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1002  1    
      3 1002  2    
      4 1002  3    
      5 1002  4    
      
      $`samples_CNS-2_250µm`
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1001 
      2 1002 
      
      $`samples_CNS-3_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-3_100µm`
      # A tibble: 1 x 1
        ID   
        <chr>
      1 1001 
      
      $`samples_CNS-3_250µm`
      # A tibble: 1 x 1
        ID   
        <chr>
      1 1001 
      
      $`samples_CNS-4_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_100µm`
      # A tibble: 1 x 1
        ID   
        <chr>
      1 1001 
      
      $`samples_CNS-4_250µm`
      # A tibble: 1 x 1
        ID   
        <chr>
      1 1001 
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 2 x 8
        ID    y     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  M     31.0   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS SC   4 4  
      2 1002  F     32.0   2020-01-01:12:02 F4M2PB_RT     fix_4C      CNS SC   3 3  
      
      $samples_CNS
      # A tibble: 7 x 4
        ID    rep   resample         reps 
        <chr> <chr> <chr>            <chr>
      1 1001  1     50µm 100µm 250µm 4 1 1
      2 1001  2     50µm 100µm 250µm 4 1 1
      3 1001  3     50µm 100µm 250µm 4 1 1
      4 1001  4     50µm 100µm 250µm 4 1 1
      5 1002  1     50µm 100µm 250µm 4 4 1
      6 1002  2     50µm 100µm 250µm 4 4 1
      7 1002  3     50µm 100µm 250µm 4 4 1
      
      $samples_SC
      # A tibble: 7 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      
      $`samples_CNS-1_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      
      $`samples_CNS-1_100µm`
      # A tibble: 5 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1002  1    
      3 1002  2    
      4 1002  3    
      5 1002  4    
      
      $`samples_CNS-1_250µm`
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1001 
      2 1002 
      
      $`samples_CNS-2_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      5 1002  1    
      6 1002  2    
      7 1002  3    
      8 1002  4    
      
      $`samples_CNS-2_100µm`
      # A tibble: 5 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1002  1    
      3 1002  2    
      4 1002  3    
      5 1002  4    
      
      $`samples_CNS-2_250µm`
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1001 
      2 1002 
      
      $`samples_CNS-3_50µm`
      # A tibble: 8 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      6 1001  2    
      7 1001  3    
      8 1001  4    
      
      $`samples_CNS-3_100µm`
      # A tibble: 5 x 2
        ID    rep  
        <chr> <chr>
      1 1002  1    
      2 1002  2    
      3 1002  3    
      4 1002  4    
      5 1001  1    
      
      $`samples_CNS-3_250µm`
      # A tibble: 2 x 1
        ID   
        <chr>
      1 1002 
      2 1001 
      
      $`samples_CNS-4_50µm`
      # A tibble: 4 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1001  4    
      
      $`samples_CNS-4_100µm`
      # A tibble: 1 x 1
        ID   
        <chr>
      1 1001 
      
      $`samples_CNS-4_250µm`
      # A tibble: 1 x 1
        ID   
        <chr>
      1 1001 
      

# datatable_read_vector() : ADD_DATA Datatables

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F     31.1  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F     32.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M     33.3  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     34.4  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      <NA>  <NA>  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F     32.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      <NA>  <NA>  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     34.4  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F     32.2  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_4C      M     34.4  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_RT      F     32.2  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     34.4  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F     32.2  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F     32.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      F     32.2  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      F     32.2  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F     32.2  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F     32.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M     34.4  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     34.4  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F     31.2  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F     31.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      F     33.3  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     34.4  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      B     31.2  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      B     31.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      F     33.3  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     34.4  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`   
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr>    
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F 1   31.1 30.1
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F 2   32.2 30.2
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M 3   33.3 30.3
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M 4   34.4 30.4
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`   
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr>    
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      <NA>  <NA>     
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F 2   32.2 30.2
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      <NA>  <NA>     
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M 4   34.4 30.4
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`   
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr>    
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      M 2   32.2 30.2
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_4C      F 4   32.4 30.4
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_RT      M 2   32.2 30.2
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      F 4   32.4 30.4
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`   
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr>    
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F 2   32.2 30.2
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F 2   32.2 30.2
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      F 2   32.2 30.2
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      F 2   32.2 30.2
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`   
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr>    
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F 2   31.2 31.2
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F 2   31.2 31.2
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M 4   33.4 33.4
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M 4   33.4 33.4
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`   
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr>    
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F 2   31.2 31.2
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F 2   31.2 31.2
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      F 4   33.4 33.4
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      F 4   33.4 33.4
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`   
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr>    
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F 2   31.2 31.2
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F 2   31.2 31.2
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      F 4   33.4 33.4
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      F 4   33.4 33.4
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, `group-dix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 13
        ID    x     `wt-g` perfuse_dt   perfusion_con `group-fix` resample reps  y    
        <chr> <chr> <chr>  <chr>        <chr>         <chr>       <chr>    <chr> <chr>
      1 1001  F     31.1   2020-01-01:~ F4M1PB_RT     fix_RT      CNS      3     <NA> 
      2 1002  F     32.2   2020-01-01:~ F4M2PB_RT     fix_RT      CNS      3     <NA> 
      3 1003  M     33.3   2020-01-01:~ F4M3PB_4C     fix_4C      <NA>     <NA>  F 2  
      4 1004  M     34.4   2020-01-01:~ F4M4PB_4C     fix_4C      <NA>     <NA>  F 2  
      # i 4 more variables: `dt-g` <chr>, derfuse_dt <chr>, derfusion_con <chr>,
      #   `group-dix` <chr>
      
      $samples_CNS
      # A tibble: 6 x 2
        ID    rep  
        <chr> <chr>
      1 1001  1    
      2 1001  2    
      3 1001  3    
      4 1002  1    
      5 1002  2    
      6 1002  3    
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     F     31.1   2020-01-01:12:11 F4M1PB_RT     dix_RT     
       2 1001  2     F     31.2   2020-01-01:12:12 F4M1PB_RT     dix_RT     
       3 1001  3     F     31.3   2020-01-01:12:13 F4M1PB_RT     dix_RT     
       4 1002  1     F     32.1   2020-01-01:12:21 F4M2PB_RT     dix_RT     
       5 1002  2     F     32.2   2020-01-01:12:22 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.3   2020-01-01:12:23 F4M2PB_RT     dix_RT     
       7 1003  1     M     33.1   2020-01-01:12:31 F4M3PB_4C     dix_4C     
       8 1003  2     M     33.2   2020-01-01:12:32 F4M3PB_4C     dix_4C     
       9 1003  3     M     33.3   2020-01-01:12:33 F4M3PB_4C     dix_4C     
      10 1004  1     M     34.1   2020-01-01:12:41 F4M4PB_4C     dix_4C     
      11 1004  2     M     34.2   2020-01-01:12:42 F4M4PB_4C     dix_4C     
      12 1004  3     M     34.3   2020-01-01:12:43 F4M4PB_4C     dix_4C     
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     F     31.1   2020-01-01:12:01 F4M1PB_RT     dix_RT     
       2 1001  2     F     31.1   2020-01-01:12:01 F4M1PB_RT     dix_RT     
       3 1001  3     F     31.1   2020-01-01:12:01 F4M1PB_RT     dix_RT     
       4 1002  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       5 1002  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       7 1003  1     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       8 1003  2     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       9 1003  3     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
      10 1004  1     M     34.4   2020-01-01:12:04 F4M4PB_4C     dix_4C     
      11 1004  2     M     34.4   2020-01-01:12:04 F4M4PB_4C     dix_4C     
      12 1004  3     M     34.4   2020-01-01:12:04 F4M4PB_4C     dix_4C     
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       2 1001  2     F     31.2   2020-01-01:12:12 F4M1PB_RT     dix_RT     
       3 1001  3     <NA>  <NA>   <NA>             <NA>          <NA>       
       4 1002  1     F     32.1   2020-01-01:12:21 F4M2PB_RT     dix_RT     
       5 1002  2     <NA>  <NA>   <NA>             <NA>          <NA>       
       6 1002  3     F     32.3   2020-01-01:12:23 F4M2PB_RT     dix_RT     
       7 1003  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       8 1003  2     M     33.2   2020-01-01:12:32 F4M3PB_4C     dix_4C     
       9 1003  3     <NA>  <NA>   <NA>             <NA>          <NA>       
      10 1004  1     M     34.1   2020-01-01:12:41 F4M4PB_4C     dix_4C     
      11 1004  2     <NA>  <NA>   <NA>             <NA>          <NA>       
      12 1004  3     M     34.3   2020-01-01:12:43 F4M4PB_4C     dix_4C     
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       2 1001  2     F     31.2   2020-01-01:12:12 F4M1PB_RT     dix_RT     
       3 1001  3     F     31.2   2020-01-01:12:12 F4M1PB_RT     dix_RT     
       4 1002  1     F     32.1   2020-01-01:12:21 F4M2PB_RT     dix_RT     
       5 1002  2     F     32.1   2020-01-01:12:21 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.3   2020-01-01:12:23 F4M2PB_RT     dix_RT     
       7 1003  1     F     32.3   2020-01-01:12:23 F4M2PB_RT     dix_RT     
       8 1003  2     M     33.2   2020-01-01:12:32 F4M3PB_4C     dix_4C     
       9 1003  3     M     33.2   2020-01-01:12:32 F4M3PB_4C     dix_4C     
      10 1004  1     M     34.1   2020-01-01:12:41 F4M4PB_4C     dix_4C     
      11 1004  2     M     34.1   2020-01-01:12:41 F4M4PB_4C     dix_4C     
      12 1004  3     M     34.3   2020-01-01:12:43 F4M4PB_4C     dix_4C     
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       2 1001  2     <NA>  <NA>   <NA>             <NA>          <NA>       
       3 1001  3     <NA>  <NA>   <NA>             <NA>          <NA>       
       4 1002  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       5 1002  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       7 1003  1     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       8 1003  2     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       9 1003  3     <NA>  <NA>   <NA>             <NA>          <NA>       
      10 1004  1     <NA>  <NA>   <NA>             <NA>          <NA>       
      11 1004  2     <NA>  <NA>   <NA>             <NA>          <NA>       
      12 1004  3     <NA>  <NA>   <NA>             <NA>          <NA>       
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       2 1001  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       3 1001  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       4 1002  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       5 1002  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       7 1003  1     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       8 1003  2     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       9 1003  3     <NA>  <NA>   <NA>             <NA>          <NA>       
      10 1004  1     <NA>  <NA>   <NA>             <NA>          <NA>       
      11 1004  2     <NA>  <NA>   <NA>             <NA>          <NA>       
      12 1004  3     <NA>  <NA>   <NA>             <NA>          <NA>       
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       2 1001  2     <NA>  <NA>   <NA>             <NA>          <NA>       
       3 1001  3     <NA>  <NA>   <NA>             <NA>          <NA>       
       4 1002  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       5 1002  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       7 1003  1     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       8 1003  2     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       9 1003  3     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
      10 1004  1     <NA>  <NA>   <NA>             <NA>          <NA>       
      11 1004  2     <NA>  <NA>   <NA>             <NA>          <NA>       
      12 1004  3     <NA>  <NA>   <NA>             <NA>          <NA>       
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     <NA>  <NA>   <NA>             <NA>          <NA>       
       2 1001  2     <NA>  <NA>   <NA>             <NA>          <NA>       
       3 1001  3     <NA>  <NA>   <NA>             <NA>          <NA>       
       4 1002  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       5 1002  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       7 1003  1     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       8 1003  2     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
       9 1003  3     M     33.3   2020-01-01:12:03 F4M3PB_4C     dix_4C     
      10 1004  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      11 1004  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      12 1004  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       2 1001  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       3 1001  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       4 1002  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       5 1002  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       7 1003  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       8 1003  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       9 1003  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      10 1004  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      11 1004  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      12 1004  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 7
         ID    rep   y     `dt-g` derfuse_dt       derfusion_con `group-dix`
         <chr> <chr> <chr> <chr>  <chr>            <chr>         <chr>      
       1 1001  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       2 1001  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       3 1001  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       4 1002  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       5 1002  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       6 1002  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       7 1003  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       8 1003  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
       9 1003  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      10 1004  1     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      11 1004  2     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      12 1004  3     F     32.2   2020-01-01:12:02 F4M2PB_RT     dix_RT     
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 12
         ID    rep   y     `dt-g` derfuse_dt    derfusion_con `group-dix` z     `ft-g`
         <chr> <chr> <chr> <chr>  <chr>         <chr>         <chr>       <chr> <chr> 
       1 1001  1     F     31.1   2020-01-01:1~ F4M1PB_RT     dix_RT      F     31.1  
       2 1001  2     F     31.2   2020-01-01:1~ F4M1PB_RT     dix_RT      F     31.1  
       3 1001  3     F     31.3   2020-01-01:1~ F4M1PB_RT     dix_RT      F     31.1  
       4 1002  1     F     32.1   2020-01-01:1~ F4M2PB_RT     dix_RT      F     31.1  
       5 1002  2     F     32.2   2020-01-01:1~ F4M2PB_RT     dix_RT      F     31.1  
       6 1002  3     F     32.3   2020-01-01:1~ F4M2PB_RT     dix_RT      F     31.1  
       7 1003  1     M     33.1   2020-01-01:1~ F4M3PB_4C     dix_4C      <NA>  <NA>  
       8 1003  2     M     33.2   2020-01-01:1~ F4M3PB_4C     dix_4C      <NA>  <NA>  
       9 1003  3     M     33.3   2020-01-01:1~ F4M3PB_4C     dix_4C      <NA>  <NA>  
      10 1004  1     M     34.1   2020-01-01:1~ F4M4PB_4C     dix_4C      <NA>  <NA>  
      11 1004  2     M     34.2   2020-01-01:1~ F4M4PB_4C     dix_4C      <NA>  <NA>  
      12 1004  3     M     34.3   2020-01-01:1~ F4M4PB_4C     dix_4C      <NA>  <NA>  
      # i 3 more variables: ferfuse_dt <chr>, ferfusion_con <chr>, `group-fix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 12
         ID    rep   y     `dt-g` derfuse_dt    derfusion_con `group-dix` z     `ft-g`
         <chr> <chr> <chr> <chr>  <chr>         <chr>         <chr>       <chr> <chr> 
       1 1001  1     F     31.1   2020-01-01:1~ F4M1PB_RT     dix_RT      F     31.1  
       2 1001  2     F     31.2   2020-01-01:1~ F4M1PB_RT     dix_RT      F     31.1  
       3 1001  3     F     31.3   2020-01-01:1~ F4M1PB_RT     dix_RT      F     31.1  
       4 1002  1     F     32.1   2020-01-01:1~ F4M2PB_RT     dix_RT      F     31.1  
       5 1002  2     F     32.2   2020-01-01:1~ F4M2PB_RT     dix_RT      F     31.1  
       6 1002  3     F     32.3   2020-01-01:1~ F4M2PB_RT     dix_RT      F     31.1  
       7 1003  1     M     33.1   2020-01-01:1~ F4M3PB_4C     dix_4C      F     31.1  
       8 1003  2     M     33.2   2020-01-01:1~ F4M3PB_4C     dix_4C      F     31.1  
       9 1003  3     M     33.3   2020-01-01:1~ F4M3PB_4C     dix_4C      F     31.1  
      10 1004  1     M     34.1   2020-01-01:1~ F4M4PB_4C     dix_4C      F     31.1  
      11 1004  2     M     34.2   2020-01-01:1~ F4M4PB_4C     dix_4C      F     31.1  
      12 1004  3     M     34.3   2020-01-01:1~ F4M4PB_4C     dix_4C      F     31.1  
      # i 3 more variables: ferfuse_dt <chr>, ferfusion_con <chr>, `group-fix` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F     31.1  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F     32.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M     33.3  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     34.4  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, group_dix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      <NA>  <NA>  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F     32.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M     33.3  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      <NA>  <NA>  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, group_dix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      F     31.1  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      F     32.2  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M     33.3  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     34.4  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, group_dix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      M     31.1  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      M     31.1  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      F     32.2  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      F     32.2  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, group_dix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      <NA>  <NA>  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      <NA>  <NA>  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M     31.1  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     31.1  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, group_dix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 11
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` y     `dt-g`
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      M     31.1  
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      M     31.1  
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      M     31.1  
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      M     31.1  
      # i 3 more variables: derfuse_dt <chr>, derfusion_con <chr>, group_dix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 11
         ID    rep   `dt-g` derfuse_dt       derfusion_con `group-dix` y     `ft-g`
         <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
       1 1001  1     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       2 1001  2     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       3 1001  3     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       4 1002  1     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      F     32.2  
       5 1002  2     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      F     32.2  
       6 1002  3     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      F     32.2  
       7 1003  1     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       8 1003  2     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       9 1003  3     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
      10 1004  1     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      F     32.2  
      11 1004  2     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      F     32.2  
      12 1004  3     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      F     32.2  
      # i 3 more variables: ferfuse_dt <chr>, ferfusion_con <chr>, group_fix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 11
         ID    rep   `dt-g` derfuse_dt       derfusion_con `group-dix` y     `ft-g`
         <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
       1 1001  1     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      <NA>  <NA>  
       2 1001  2     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      <NA>  <NA>  
       3 1001  3     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      <NA>  <NA>  
       4 1002  1     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
       5 1002  2     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
       6 1002  3     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
       7 1003  1     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      <NA>  <NA>  
       8 1003  2     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      <NA>  <NA>  
       9 1003  3     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      <NA>  <NA>  
      10 1004  1     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
      11 1004  2     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
      12 1004  3     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
      # i 3 more variables: ferfuse_dt <chr>, ferfusion_con <chr>, group_fix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 11
         ID    rep   `dt-g` derfuse_dt       derfusion_con `group-dix` y     `ft-g`
         <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr> <chr> 
       1 1001  1     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       2 1001  2     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       3 1001  3     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       4 1002  1     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
       5 1002  2     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
       6 1002  3     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
       7 1003  1     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       8 1003  2     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
       9 1003  3     13.3   2020-01-01:11:03 F4M1PB_RT     dix_RT      M     31.1  
      10 1004  1     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
      11 1004  2     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
      12 1004  3     22.3   2020-01-01:12:23 F4M2PB_RT     dix_4C      M     31.1  
      # i 3 more variables: ferfuse_dt <chr>, ferfusion_con <chr>, group_fix <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt     perfusion_con `group-fix` delip_con delip_dt
        <chr> <chr> <chr>  <chr>          <chr>         <chr>       <chr>     <chr>   
      1 1001  F     30.0   2020-01-01:12~ F4M1PB_RT     fix_RT      HT_RT MT~ 2024-01~
      2 1002  F     31.1   2020-01-01:12~ F4M2PB_RT     fix_RT      HT_RT MT~ 2024-01~
      3 1003  M     32.2   2020-01-01:12~ F4M3PB_4C     fix_4C      HT_RT MT~ 2024-01~
      4 1004  M     33.3   2020-01-01:12~ F4M4PB_4C     fix_4C      HT_RT MT~ 2024-01~
      

# datatable_read_vector() : GROUP Datatables

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix `group-solvent-inc`
        <chr> <chr> <chr> <chr>            <chr>         <chr>     <chr>              
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT    1Hr                
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT    2Hr                
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C    4Hr                
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C    1Hr                
      # i 1 more variable: `group-ab-conc` <chr>
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 4
         ID    rep   `group-solvent-inc` `group-ab-conc`
         <chr> <chr> <chr>               <chr>          
       1 1001  1     1Hr                 1mg/mL         
       2 1001  2     2Hr                 1mg/mL         
       3 1001  3     3Hr                 1mg/mL         
       4 1002  1     1Hr                 0.5mg/mL       
       5 1002  2     2Hr                 0.5mg/mL       
       6 1002  3     3Hr                 0.5mg/mL       
       7 1003  1     1Hr                 1mg/mL         
       8 1003  2     2Hr                 1mg/mL         
       9 1003  3     3Hr                 1mg/mL         
      10 1004  1     1Hr                 0.5mg/mL       
      11 1004  2     2Hr                 0.5mg/mL       
      12 1004  3     3Hr                 0.5mg/mL       
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 4
         ID    rep   `group-solvent-inc` `group-ab-conc`
         <chr> <chr> <chr>               <chr>          
       1 1001  1     1Hr                 1mg/mL         
       2 1001  2     1Hr                 1mg/mL         
       3 1001  3     1Hr                 1mg/mL         
       4 1002  1     2Hr                 0.5mg/mL       
       5 1002  2     2Hr                 0.5mg/mL       
       6 1002  3     2Hr                 0.5mg/mL       
       7 1003  1     4Hr                 1mg/mL         
       8 1003  2     4Hr                 1mg/mL         
       9 1003  3     4Hr                 1mg/mL         
      10 1004  1     1Hr                 0.5mg/mL       
      11 1004  2     1Hr                 0.5mg/mL       
      12 1004  3     1Hr                 0.5mg/mL       
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     `wt-g` perfuse_dt       perfusion_con `group-fix` resample reps 
        <chr> <chr> <chr>  <chr>            <chr>         <chr>       <chr>    <chr>
      1 1001  F     31.1   2020-01-01:12:01 F4M1PB_RT     fix_RT      CNS      3    
      2 1002  F     32.2   2020-01-01:12:02 F4M2PB_RT     fix_RT      CNS      3    
      3 1003  M     33.3   2020-01-01:12:03 F4M3PB_4C     fix_4C      CNS      3    
      4 1004  M     34.4   2020-01-01:12:04 F4M4PB_4C     fix_4C      CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 4
         ID    rep   `group-solvent-inc` `group-ab-conc`
         <chr> <chr> <chr>               <chr>          
       1 1001  1     1Hr                 1mg/mL         
       2 1001  2     1Hr                 1mg/mL         
       3 1001  3     1Hr                 1mg/mL         
       4 1002  1     2Hr                 0.5mg/mL       
       5 1002  2     2Hr                 0.5mg/mL       
       6 1002  3     2Hr                 0.5mg/mL       
       7 1003  1     4Hr                 1mg/mL         
       8 1003  2     4Hr                 1mg/mL         
       9 1003  3     4Hr                 1mg/mL         
      10 1004  1     1Hr                 0.5mg/mL       
      11 1004  2     1Hr                 0.5mg/mL       
      12 1004  3     1Hr                 0.5mg/mL       
      

# datatable_read_vector() : EXPORT Datatables

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 7
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix export             
        <chr> <chr> <chr> <chr>            <chr>         <chr>     <chr>              
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT    ../dest-note/dest-~
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT    ../dest-note/dest-~
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C    ../dest-note/dest-~
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C    ../dest-note/dest-~
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 7
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix export             
        <chr> <chr> <chr> <chr>            <chr>         <chr>     <chr>              
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT    ../../../../../pro~
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT    ../../../../../pro~
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C    ../../../../../pro~
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C    ../../../../../pro~
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 7
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix export             
        <chr> <chr> <chr> <chr>            <chr>         <chr>     <chr>              
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT    ../../../../../pro~
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT    ../../../../../pro~
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C    ../../../../../pro~
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C    ../../../../../pro~
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix resample reps 
        <chr> <chr> <chr> <chr>            <chr>         <chr>     <chr>    <chr>
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT    CNS      3    
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT    CNS      3    
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C    CNS      3    
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C    CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 3
         ID    rep   export                    
         <chr> <chr> <chr>                     
       1 1001  1     ../dest-note/dest-note~001
       2 1001  2     ../dest-note/dest-note~002
       3 1001  3     ../dest-note/dest-note~003
       4 1002  1     ../dest-note/dest-note~001
       5 1002  2     ../dest-note/dest-note~002
       6 1002  3     ../dest-note/dest-note~003
       7 1003  1     ../dest-note/dest-note~001
       8 1003  2     ../dest-note/dest-note~002
       9 1003  3     ../dest-note/dest-note~003
      10 1004  1     ../dest-note/dest-note~001
      11 1004  2     ../dest-note/dest-note~002
      12 1004  3     ../dest-note/dest-note~003
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix resample reps 
        <chr> <chr> <chr> <chr>            <chr>         <chr>     <chr>    <chr>
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT    CNS      3    
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT    CNS      3    
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C    CNS      3    
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C    CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 3
         ID    rep   export                    
         <chr> <chr> <chr>                     
       1 1001  1     ../dest-note/dest-note~001
       2 1001  2     ../dest-note/dest-note~001
       3 1001  3     ../dest-note/dest-note~001
       4 1002  1     ../dest-note/dest-note~001
       5 1002  2     ../dest-note/dest-note~001
       6 1002  3     ../dest-note/dest-note~001
       7 1003  1     ../dest-note/dest-note~001
       8 1003  2     ../dest-note/dest-note~001
       9 1003  3     ../dest-note/dest-note~001
      10 1004  1     ../dest-note/dest-note~001
      11 1004  2     ../dest-note/dest-note~001
      12 1004  3     ../dest-note/dest-note~001
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 8
        ID    x     wt_g  perfuse_dt       perfusion_con group_fix resample reps 
        <chr> <chr> <chr> <chr>            <chr>         <chr>     <chr>    <chr>
      1 1001  F     30.0  2020-01-01:12:01 F4M1PB_RT     fix_RT    CNS      3    
      2 1002  F     31.1  2020-01-01:12:02 F4M2PB_RT     fix_RT    CNS      3    
      3 1003  M     32.2  2020-01-01:12:03 F4M3PB_4C     fix_4C    CNS      3    
      4 1004  M     33.3  2020-01-01:12:04 F4M4PB_4C     fix_4C    CNS      3    
      
      $samples_CNS
      # A tibble: 12 x 3
         ID    rep   export                    
         <chr> <chr> <chr>                     
       1 1001  1     ../dest-note/dest-note~001
       2 1001  2     ../dest-note/dest-note~001
       3 1001  3     ../dest-note/dest-note~001
       4 1002  1     ../dest-note/dest-note~001
       5 1002  2     ../dest-note/dest-note~001
       6 1002  3     ../dest-note/dest-note~001
       7 1003  1     ../dest-note/dest-note~001
       8 1003  2     ../dest-note/dest-note~001
       9 1003  3     ../dest-note/dest-note~001
      10 1004  1     ../dest-note/dest-note~001
      11 1004  2     ../dest-note/dest-note~001
      12 1004  3     ../dest-note/dest-note~001
      

# datatable_read_vector() : IMPORT Datatables

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 2
        ID    import                        
        <chr> <chr>                         
      1 1001  ../source-note/source-note~001
      2 1002  ../source-note/source-note~001
      3 1003  ../source-note/source-note~001
      4 1004  ../source-note/source-note~001
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 4 x 2
        ID    import                                                                  
        <chr> <chr>                                                                   
      1 1001  ../../../../../programme-path/project-doc-path/project-note-path/note-s~
      2 1002  ../../../../../programme-path/project-doc-path/project-note-path/note-s~
      3 1003  ../../../../../programme-path/project-doc-path/project-note-path/note-s~
      4 1004  ../../../../../programme-path/project-doc-path/project-note-path/note-s~
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 12 x 3
         ID      rep import                        
         <chr> <int> <chr>                         
       1 1001      1 ../source-note/source-note~001
       2 1001      2 ../source-note/source-note~001
       3 1001      3 ../source-note/source-note~001
       4 1002      1 ../source-note/source-note~001
       5 1002      2 ../source-note/source-note~001
       6 1002      3 ../source-note/source-note~001
       7 1003      1 ../source-note/source-note~001
       8 1003      2 ../source-note/source-note~001
       9 1003      3 ../source-note/source-note~001
      10 1004      1 ../source-note/source-note~001
      11 1004      2 ../source-note/source-note~001
      12 1004      3 ../source-note/source-note~001
      

---

    Code
      datatable_read_vector(contents)
    Output
      $samples
      # A tibble: 12 x 3
         ID      rep import                        
         <chr> <int> <chr>                         
       1 1001      1 ../source-note/source-note~001
       2 1001      2 ../source-note/source-note~001
       3 1001      3 ../source-note/source-note~001
       4 1002      1 ../source-note/source-note~002
       5 1002      2 ../source-note/source-note~002
       6 1002      3 ../source-note/source-note~002
       7 1003      1 ../source-note/source-note~003
       8 1003      2 ../source-note/source-note~003
       9 1003      3 ../source-note/source-note~003
      10 1004      1 ../source-note/source-note~004
      11 1004      2 ../source-note/source-note~004
      12 1004      3 ../source-note/source-note~004
      

