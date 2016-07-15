
7/15/2016:
  Kristi helped with the ldt.config file in 
/discover/nobackup/ytian/7.0/test-gdas/development/run-noah36-1-deg

and now the parameters on 1.25x0.942 grid all looked fine. 

Then ran LIS under /home/ytian/7.0-development/test-gdas/development/run-noah36-1-deg
and now all the forcing variables look OK. 


7/13/2016: 
   Trying the newest version of LDT for the 1.25x0.942 grid: 

/home/ytian/LDT-7.0/development/

~/7.0-development/test-gdas/development/run-noah36-1-deg$ /home/ytian/LDT-7.0/development/LDT ldt.config-1.25

and same problem with the previous version /home/ytian/LDT-7.0/LDT_public_release_7.1rp1. 


7/12/2016: 

 It seems the CLM4.5 benchmark resolution of 1.25x0.942 is causing problems with 
GDAS forcing data (currently only GDAS forcing supports upscaling to coarser grids). 
To track down the problem, got a fresh checkout of LIS code to: 

/discover/nobackup/ytian/7.0/test-gdas/development

and set up a 1.25x0.942 global (60S - 90N) run under: 

/discover/nobackup/ytian/7.0/test-gdas/development/run-noah36-1-deg

Results: LIS ran. But the results are not on the global domain -- they are on the South America continent
projected to the global domain. 

Problem figured out: in ldt.config-1.25, 
      Landmask spatial transform:      none                   # none | mode | neighbor
should be changed to 
      Landmask spatial transform:      mode                   # none | mode | neighbor

But this change only fixed the problem with landmask. Other parameters are still on S. America. 


6/20/2016
  Current status: 
1. CLM4.5 implementation done and ran without crashing. Currently crashing due to errors in met-forcing input (see 2 below). 
2. The input forcing data did not look right. Tried GDAS and MERRA2, with same issue. 
3. Tried ldt file with grid center shifted or not. Did not solve issue. 
4. Changed spatial interpolation option from "linear" to "neighbor". Did not solve issue. 
5. Pretty sure the grid res: 1.25x0.942 caused the problem. Not sure what went wrong though. 




2/2/2016
This is a test case that uses: 
  (a) the GDAS forcing 
  (b) using the bilinear Interpolation
  (c) uses the lapse-rate based elevation correction using GTOPO30 
      elevation data
  (d) CLM version 2.0 land surface model
  (e) North American domain at 1/4 degree spatial resolution.
  (f) a time period from Oct 29 2002, hr 1 to Oct 31, 2002, hr 1 (This 
      period is chosen since GDAS switches resolution on Oct 29, hr 12) 

This directory contains: 
  (a) This README file, 
  (b) the lis.config file used for this test case. (This file should be 
      edited to make sure that the locations of the parameter and 
      forcing files are specified correctly.) 
  (c) A sample grads control file that can be used to visualize the 
      output. (output.ctl)

To run this test case: 
  (a) Generate the LIS executable. 
  (b) Download the sample input data from: 
	http://lis.gsfc.nasa.gov/Source/testcases  
      the testcase corresponding to CLM2 LSM TEST
  (c) Run the LIS executable using the lis.config file and the sample 
      input data. 
  (d) View the binary output using the sample grads file. 
       
Caveats: 
  (a) Please note that this is a simple functional test and the output
      from the testcase is not expected to be used for any scientific 
      evaluation. 
