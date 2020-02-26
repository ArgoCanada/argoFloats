## Below is the start of the file.
##> # Title : Merged-Profile directory file of the Argo Global Data Assembly Center
##> # Description : The directory file describes all individual merge-profile files of the argo GDAC ftp site.
##> # Project : ARGO
##> # Format version : 2.1
##> # Date of update : 20200226143020
##> # FTP root number 1 : ftp://ftp.ifremer.fr/ifremer/argo/dac
##> # FTP root number 2 : ftp://usgodae.org/pub/outgoing/argo/dac
##> # GDAC node : NRL-MRY
##> file,date,latitude,longitude,ocean,profiler_type,institution,date_update,
##> aoml/1900722/profiles/MD1900722_001.nc,20061022021624,-40.316,73.389,I,846,AO,PRES TEMP PSAL DOXY,DDDR,20161021233045
##> aoml/1900722/profiles/MD1900722_002.nc,20061101064423,-40.390,73.528,I,846,AO,PRES TEMP PSAL DOXY,DDDR,20161021224705

d <- read.csv("argo_merge-profile_index.txt.gz", skip=8, header=FALSE)

