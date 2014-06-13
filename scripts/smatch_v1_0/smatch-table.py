#!/usr/bin/env python

import amr
import sys
import subprocess
import smatch_modified
import os
import random
import time
#import optparse
#import argparse #argparse only works for python 2.7. If you are using older versin of Python, you can use optparse instead.
#import locale

ERROR_LOG=sys.stderr

verbose=False

isi_dir_pre="/nfs/web/isi.edu/cgi-bin/div3/mt/save-amr"

"""
Get the annotator name list based on a list of files
Args:
   file_dir: AMR file folder
   files: a list of AMR names, e.g. nw_wsj_0001_1
Return:
   a list of user names who annotate all the files
"""
def get_names(file_dir,files):
    #for each user, check if they have files available
    #return user name list
    total_list=[]
    name_list=[]
    get_sub=False
    for path,subdir,dir_files in os.walk(file_dir):
  #      print path
        if get_sub==False:
           total_list=subdir[:]
	   get_sub=True
	else:
	   break
    for user in total_list:
        #print user
	has_file=True 
        for file in files:
	#  print file
	  file_path=file_dir+user+"/"+file+".txt"
#	  print file_path
	  if not os.path.exists(file_path):
	     has_file=False
	     break   
        if has_file==True:
	   name_list.append(user)
   # print name_list
    if len(name_list)==0:
       print >> ERROR_LOG,"********Error: Cannot find any user who completes the files*************"
    return name_list
"""
Compute the smatch scores for a file list between two users 
Args:
   user1: user 1 name
   user2: user 2 name
   file_list: file list
   dir_pre: the file location prefix
   start_num: the number of restarts in smatch
Returns:
   smatch f score.
"""
def compute_files(user1,user2,file_list,dir_pre,start_num):
   #print file_list
   #print user1, user2
   match_total=0
   test_total=0
   gold_total=0
   for fi in file_list:
     file1=dir_pre+user1+"/"+fi+".txt"
     file2=dir_pre+user2+"/"+fi+".txt"
     #print file1,file2
     if not os.path.exists(file1):  
       print >> ERROR_LOG,"*********Error: ", file1, "does not exist*********"
       return -1.00
     if not os.path.exists(file2):  
       print >> ERROR_LOG,"*********Error: ", file2, "does not exist*********"
       return -1.00
     try:
       file1_h=open(file1,"r")
       file2_h=open(file2,"r")
     except:
       print >> ERROR_LOG, "Cannot open the files", file1, file2
     cur_amr1=smatch_modified.get_amr_line(file1_h)
     cur_amr2=smatch_modified.get_amr_line(file2_h)
     if(cur_amr1==""):
        print >> ERROR_LOG, "AMR 1 is empty"
        continue      
     if(cur_amr2==""):
        print >> ERROR_LOG, "AMR 2 is empty"
        continue
     amr1=amr.AMR.parse_AMR_line(cur_amr1)
     amr2=amr.AMR.parse_AMR_line(cur_amr2) 
     test_label="a"
     gold_label="b"
     amr1.rename_node(test_label)
     amr2.rename_node(gold_label)
     (test_inst,test_rel1,test_rel2)=amr1.get_triples2()
     (gold_inst,gold_rel1,gold_rel2)=amr2.get_triples2()
     if verbose:
       print >> ERROR_LOG,"Instance triples of file 1:",len(test_inst)
       print >> ERROR_LOG,test_inst
       print >> sys.stderr,"Relation triples of file 1:",len(test_rel1)+len(test_rel2)
       print >>sys.stderr,test_rel1
       print >> sys.stderr,test_rel2
       print >> ERROR_LOG,"Instance triples of file 2:",len(gold_inst)
       print >> ERROR_LOG,gold_inst
       print >> sys.stderr,"Relation triples of file 2:",len(gold_rel1)+len(gold_rel2)
       print >> sys.stderr,gold_rel1
       print >> sys.stderr,gold_rel2
     if len(test_inst)<len(gold_inst):
       (best_match,best_match_num)=smatch_modified.get_fh(test_inst,test_rel1,test_rel2,gold_inst,gold_rel1,gold_rel2,test_label,gold_label)
       if verbose:
          print >> ERROR_LOG, "best match number",best_match_num
          print >>ERROR_LOG,"Best Match:",smatch_modified.print_alignment(best_match,test_inst,gold_inst)
     else:
       (best_match,best_match_num)=smatch_modified.get_fh(gold_inst,gold_rel1,gold_rel2,test_inst,test_rel1,test_rel2,gold_label,test_label)
       if verbose:
          print >> ERROR_LOG, "best match number",best_match_num
          print >>ERROR_LOG,"Best Match:",smatch_modified.print_alignment(best_match,gold_inst,test_inst,True)
     #(match_num,test_num,gold_num)=smatch.get_match(tmp_filename1,tmp_filename2,start_num)
     #print match_num,test_num,gold_num
    # print best_match_num
    # print len(test_inst)+len(test_rel1)+len(test_rel2)
    # print len(gold_inst)+len(gold_rel1)+len(gold_rel2) 
     match_total+=best_match_num
     test_total+=len(test_inst)+len(test_rel1)+len(test_rel2)
     gold_total+=len(gold_inst)+len(gold_rel1)+len(gold_rel2)
     smatch_modified.match_num_dict.clear()
   (precision,recall,f_score)=smatch_modified.compute_f(match_total,test_total,gold_total)
   return "%.2f" % f_score


def get_max_width(table,index):
   return max([len(str(row[index])) for row in table])
"""
Print a table
"""
def pprint_table(table):
   col_paddings=[]
   for i in range(len(table[0])):
     col_paddings.append(get_max_width(table,i))
   for row in table:
     print row[0].ljust(col_paddings[0]+1),
     for i in range(1,len(row)):
         col = str(row[i]).rjust(col_paddings[i]+2)
	 print col,   
     print "\n"

def print_help():
  print "Smatch Calculator Program Help"
  print "This program prints the smatch score of the two files"
  print "Command line arguments:"
  print "-h: Show help (Other options won't work if you use -h)"
  print "smatch-table.py -h"
  print "Usage: smatch-table.py file_list (-f list_file) [ -p user_list ] [-r number of starts]"
  print "File list is AMR file ids separated by a blank space"
  print "Example: smatch-table.py nw_wsj_0001_1 nw_wsj_0001_2" 
  print "Or use -f list_file to indicate a file which contains one line of file names, separated by a blank space" 
  print "Example: smatch.py -f file"
  print "-p: (Optional) user list to list the user name in the command line, after the file list. Otherwise the program automatically searches for the users who completes all AMRs you want."
  print "Example: smatch.py -f file -p user1 user2"
  print "Example: smatch.py nw_wsj_0001_1 nw_wsj_0001_2 -p user1 user2"
  print "-r: (Optional) the number of random starts(higher number may results in higher accuracy and slower speed (default number of starts: 10)"
  print "Example: smatch.py -f file -p user1 user2 -r 20"
 # print "-d: detailed output, including alignment and triples of the two files"
 # print "Example (if you want to use all options): smatch.py file1 file2 -d -r 20"
  print "Contact shucai@isi.edu for additional help"


def build_arg_parser():
    """Build an argument parser using argparse"""
    parser=argparse.ArgumentParser(description="Smatch table calculator -- arguments")
    parser.add_argument("--fl",type=argparse.FileType('r'),help='AMR ID list file')
    parser.add_argument('-f',nargs='+',help='AMR IDs (at least one)')
    parser.add_argument("-p",nargs='*',help="User list (can be none)")
    parser.add_argument("--fd",default=isi_dir_pre,help="AMR File directory. Default=location on isi machine")
    #parser.add_argument("--cd",default=os.getcwd(),help="(Dependent) code directory. Default: current directory")
    parser.add_argument('-r',type=int,default=4,help='Restart number (Default:4)')
    parser.add_argument('-v',action='store_true',help='Verbose output (Default:False)')
    return parser
"""
Callback function to handle variable number of arguments in optparse
"""
def cb(option, opt_str, value, parser):
     args=[]
     args.append(value)
     for arg in parser.rargs:
         if arg[0] != "-":
	      args.append(arg)
         else:
	      del parser.rargs[:len(args)]
              break
     if getattr(parser.values, option.dest):
        args.extend(getattr(parser.values, option.dest))
     setattr(parser.values, option.dest, args)

def build_arg_parser2():
    """Build an argument parser using optparse"""
    usage_str="Smatch table calculator -- arguments"
    parser=optparse.OptionParser(usage=usage_str)
    parser.add_option("--fl",dest="fl",type="string",help='AMR ID list file')
    parser.add_option("-f",dest="f",type="string",action="callback",callback=cb,help="AMR IDs (at least one)")
    parser.add_option("-p",dest="p",type="string",action="callback",callback=cb,help="User list")
    parser.add_option("--fd",dest="fd",type="string",help="file directory")
    #parser.add_option("--cd",dest="cd",type="string",help="code directory")
    parser.add_option("-r","--restart",dest="r",type="int",help='Restart number (Default: 4)')
    parser.add_option("-v","--verbose",action='store_true',dest="v",help='Verbose output (Default:False)')
    parser.set_defaults(r=4,v=False,ms=False,fd=isi_dir_pre)
    return parser

def check_args(args):
  """Check if the arguments are valid"""
  if not os.path.exists(args.fd):
     print >> ERROR_LOG, "Not a valid path", args.fd
     return ([],[],False)
  #if not os.path.exists(args.cd):
   #  print >> ERROR_LOG,"Not a valid path", args.cd
   #  return ([],[],False)
  amr_ids=[]
  if args.fl is not None:
     #we already ensure the file can be opened and opened the file
     file_line=args.fl.readline()
     amr_ids=file_line.strip().split()
  elif args.f is None:
     print >> ERROR_LOG, "No AMR ID was given"
     return ([],[],False)
  else:
     amr_ids=args.f
  names=[]
  check_name=True
  if args.p is None:
     names=get_names(args.fd,amr_ids)
     check_name=False #no need to check names
     if len(names)==0:
        print >> ERROR_LOG, "Cannot find any user who tagged these AMR"
        return ([],[],False)
  else:
     names=args.p
  if names==[]:
     print >> ERROR_LOG, "No user was given"
     return ([],[],False)
  if len(names)==1:
     print >> ERROR_LOG, "Only one user is given. Smatch calculation requires at least two users."
     return ([],[],False)
  if "consensus" in names:
     con_index=names.index("consensus")
     names.pop(con_index)
     names.append("consensus")
  #check if all the AMR_id and user combinations are valid
  if check_name:
     pop_name=[]
     for i,name in enumerate(names):
       for amr in amr_ids:
         amr_path=args.fd+name+"/"+amr+".txt"
         if not os.path.exists(amr_path):
            print >> ERROR_LOG, "User",name,"fails to tag AMR",amr
            pop_name.append(i)
            break
     if len(pop_name)!=0:
       pop_num=0
       for p in pop_name:
        print >> ERROR_LOG, "Deleting user",names[p-pop_num],"from the name list"
        names.pop(p-pop_num)
        pop_num+=1
     if len(names)<2:
        print >> ERROR_LOG, "Not enough users to evaluate. Smatch requires >2 users who tag all the AMRs"
        return ("","",False) 
  return (amr_ids,names,True)

def main(args):
   """Main Function"""
   (ids,names,result)=check_args(args)
   if args.v:
      verbose=True
   if not result:
      return 0
   acc_time=0
   len_name=len(names)
   table=[]
   for i in range(0,len_name+1):
     table.append([])
   table[0].append("")
   for i in range(0,len_name):
     table[0].append(names[i])
   for i in range(0,len_name):
     table[i+1].append(names[i])
     for j in range(0,len_name):
       if i!=j:
          start=time.clock()
	  table[i+1].append(compute_files(names[i],names[j],ids,args.fd,args.r))
          end=time.clock()
          if table[i+1][-1]!=-1.0:
             acc_time+=end-start
	  #if table[i+1][-1]==-1.0:
	  #   sys.exit(1)
       else:
          table[i+1].append("")
   #check table
   for i in range(0,len_name+1):
     for j in range(0,len_name+1):
       if i!=j:
          if table[i][j]!=table[j][i]:
             if table[i][j]>table[j][i]:
	        table[j][i]=table[i][j]
             else:
	        table[i][j]=table[j][i]
   pprint_table(table) 
   return acc_time


if __name__=="__main__":
  # acc_time=0 #accumulated time
   whole_start=time.clock()
   parser=None
   args=None
   if sys.version_info[:2]!=(2,7):
      #requires version >=2.3!
      if sys.version_info[0]!=2 or sys.version_info[1]<5:
         print >> ERROR_LOG, "This prgram requires python 2.5 or later to run. "
         exit(1) 
      import optparse
      parser=build_arg_parser2();
      (args,opts)=parser.parse_args()
      file_handle=None
      if args.fl is not None:
         try:
           file_handle=open(args.fl,"r")
           args.fl=file_handle
         except:
           print >> ERROR_LOG, "The ID list file",args.fl,"does not exist"
           args.fl=None
#      print args
   else: #version 2.7
      import argparse
      parser=build_arg_parser()
      args=parser.parse_args()
   #Regularize fd and cd representation
   if args.fd[-1]!="/":
      args.fd=args.fd+"/"
   #if args.cd[-1]!="/":
   #   args.cd=args.cd+"/"
   acc_time=main(args)
   whole_end=time.clock()
   whole_time=whole_end-whole_start
#   print >> ERROR_LOG, "Accumulated time", acc_time 
#   print >> ERROR_LOG, "whole time", whole_time
#   print >> ERROR_LOG, "Percentage", float(acc_time)/float(whole_time)
 
