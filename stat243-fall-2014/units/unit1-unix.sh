#######################################
# 2: Version control
#######################################

# initial download
git clone https://github.com/berkeley-stat243/stat243-fall-2014

# updating local copy to reflect changes in the remote copy
cd /to/a/directory/within/the/local/repository
git pull

#######################################
# 3: Files and directories
#######################################

# login and copying to a remote machine

ssh -X radagast
ssh -X paciorek@radagast.berkeley.edu

pwd

ls
ls -a
ls -l
ls -lrt

cd /accounts/gen/vis/paciorek/teaching/243
cd ~paciorek/teaching/243
cd ~/teaching/243
cd 243 # provided I am in 'teaching'
cd ..
cd -

cp
cp -r
cp -rp # preserves timestamps and other metainfo (VERY handy for tracing your workflows if you move files between machines)
mkdir
rm
rm -r  # recursively remove a whole directory structure
rm -rf # CAREFUL! In particular, make sure you know what directory you are in

# file transfer between machines
scp file.txt paciorek@radagast.berkeley.edu:~/research/.
scp paciorek@radagast.berkeley.edu:/data/file.txt ~/research/renamed.txt

# changing permissions on a file
chmod ugo+x myProg # myProg should be compiled code or a shell script

chmod ugo+rw code.q

chmod go-w myThesisCode.q


zip files.zip a.txt b.txt c.txt
gzip a.txt b.txt c.txt # will create a.txt.gz, b.txt.gz,  c.txt.gz
tar -cvf files.tar myDirectory
tar -cvzf files.tgz myDirectory  
tar -xvf files.tar
tar -xvzf files.tgz
gzip -cd file.gz | less
zcat file.zip | less

# disk usage
df -h
du -h
quota -s

##########################################
# 4: A variety of UNIX tools/capabilities
##########################################

man cp

which R
which matlab
which maple

emacs -nw file.txt
# let's play around with some of the keystrokes in emacs

/proc/meminfo
/proc/cpuinfo
/etc/issue

grep processor /proc/cpuinfo
