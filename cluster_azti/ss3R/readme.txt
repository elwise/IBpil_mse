####################################################
### 01/04/2019
### lcitores: notes on how to run ss32flbeia funtion in flbeia
####################################################

- Choose the appropiate ss3 executable and call it SS3.exe (in the assess_ref folder there are three different ones, ss3_win, ss3_linux, ss3_laura)

- Give all the permissions to the ss3 executable in the cluster (right-click on the ss3.exe file, permissions and tick all of them) 

- Choose the correct .ctl file. (in the assess_ref folder there are two ctl file, sardine_orig.ctl is the one from the last WG, sardine.ctl is the modified one (restricting the last age slecetivity) to reach convergence.

- Be sure that in the assess.ctrl the assess_dir is the correct one, providint the full path (i.e. assess.ctrl[["PIL"]]$control$assess_dir <- "C:/use/GitHub/FLBEIA_mseIBpil/ss3R/")

