#!/usr/bin/env python
# encoding: utf-8
"""
smatch.py 

Modified by Jeffrey Flanigan to print more digits of
precision/recall/f1

Author: Shu Cai
Copyright(c) 2012. All rights reserved.
"""
import sys
import os
import time
import random
import amr
#import optparse
#import argparse #argparse only works for python 2.7. If you are using older versin of Python, you can use optparse instead.

iter_num=5  #global variable, total number of iteration

verbose=False #global variable, verbose output control

single_score=True #global variable, single score output control

pr_flag=False #global variable, output precision and recall

ERROR_LOG=sys.stderr

match_num_dict={} #key: match number tuples	value: the matching number

def get_amr_line(input_f):
    """Read the amr file. AMRs are separated by a blank line."""
    cur_amr=[]
    has_content=False
    for line in input_f:
      if line[0]=="(" and len(cur_amr)!=0:
         cur_amr=[]
      if line.strip()=="":
         if not has_content:
            continue
         else:
            break
      elif line.strip().startswith("#"):
        # omit the comment in the AMR file
        continue
      else:
         has_content=True
         cur_amr.append(line.strip())
    return "".join(cur_amr)

def build_arg_parser():
    """Build an argument parser using argparse"""
    parser=argparse.ArgumentParser(description="Smatch calculator -- arguments")
    parser.add_argument('-f',nargs=2,required=True,type=argparse.FileType('r'),help='Two files containing AMR pairs. AMRs in each file are separated by a single blank line')
    parser.add_argument('-r',type=int,default=4,help='Restart number (Default:4)')
    parser.add_argument('-v',action='store_true',help='Verbose output (Default:False)')
    parser.add_argument('--ms',action='store_true',default=False,help='Output multiple scores (one AMR pair a score) instead of a single document-level smatch score (Default: False)')
    parser.add_argument('--pr',action='store_true',default=False,help="Output precision and recall as well as the f-score. Default: false")
    return parser


def build_arg_parser2():
    """Build an argument parser using optparse"""
    usage_str="Smatch calculator -- arguments"
    parser=optparse.OptionParser(usage=usage_str)
    #parser.add_option("-h","--help",action="help",help="Smatch calculator -- arguments")
    parser.add_option("-f","--files",nargs=2,dest="f",type="string",help='Two files containing AMR pairs. AMRs in each file are separated by a single blank line. This option is required.')
    parser.add_option("-r","--restart",dest="r",type="int",help='Restart number (Default: 4)')
    parser.add_option("-v","--verbose",action='store_true',dest="v",help='Verbose output (Default:False)')
    parser.add_option("--ms","--multiple_score",action='store_true',dest="ms",help='Output multiple scores (one AMR pair a score) instead of a single document-level smatch score (Default: False)')
    parser.add_option('--pr',"--precision_recall",action='store_true',dest="pr",help="Output precision and recall as well as the f-score. Default: false")
    parser.set_defaults(r=4,v=False,ms=False,pr=False) 
    return parser
 

def compute_pool(test_instance,test_relation1,test_relation2,gold_instance,gold_relation1,gold_relation2,test_label,gold_label):
    """
    compute the possible variable matching candidate (the match which may result in 1)
    Args:
      test_instance: intance triples in AMR 1
      test_relation1: relation triples which contain only one variable in AMR 1
      test_relation2: relation triples which contain two variables in AMR 1
      gold_instance: instance triples in AMR 2
      gold_relation1: relation triples which contain only one variable in AMR 2
      gold_relations: relation triples which contain two variables in AMR 2
      test_label: the prefix of the variable in AMR 1, e.g. a (variable a1, a2, a3...)
      gold_label: the prefix of the variable in AMR 2, e.g. b (variable b1, b2, b3...)
    Returns:
      candidate_match: a list of candidate mapping variables. Each entry contains a set of the variables the variable can map to.
      weight_dict: a dictionary which contains the matching triple number of every pair of variable mapping. """
    len_test_inst=len(test_instance)
    len_gold_inst=len(gold_instance)
    len_test_rel1=len(test_relation1)
    len_gold_rel1=len(gold_relation1)
    len_test_rel2=len(test_relation2)
    len_gold_rel2=len(gold_relation2)
    candidate_match=[]
    weight_dict={}
    for i in range(0,len_test_inst):
      candidate_match.append(set())
    for i in range(0,len_test_inst):
      for j in range(0,len_gold_inst):
        if test_instance[i][0].lower()==gold_instance[j][0].lower() and test_instance[i][2].lower()==gold_instance[j][2].lower():
           var1_num=int(test_instance[i][1][len(test_label):])
           var2_num=int(gold_instance[j][1][len(gold_label):])
           candidate_match[var1_num].add(var2_num)
           cur_k=(var1_num,var2_num)
           if cur_k in weight_dict:
              weight_dict[cur_k][-1]+=1
           else:
              weight_dict[cur_k]={}
              weight_dict[cur_k][-1]=1
    for i in range(0,len_test_rel1):
      for j in range(0,len_gold_rel1):
        if test_relation1[i][0].lower()==gold_relation1[j][0].lower() and test_relation1[i][2].lower()==gold_relation1[j][2].lower():
           var1_num=int(test_relation1[i][1][len(test_label):])
           var2_num=int(gold_relation1[j][1][len(gold_label):])
           candidate_match[var1_num].add(var2_num)
           cur_k=(var1_num,var2_num)
           if cur_k in weight_dict:
              weight_dict[cur_k][-1]+=1
           else:
              weight_dict[cur_k]={}
              weight_dict[cur_k][-1]=1
    for i in range(0,len_test_rel2):
      for j in range(0,len_gold_rel2):
        if test_relation2[i][0].lower()==gold_relation2[j][0].lower():
           var1_num_test=int(test_relation2[i][1][len(test_label):])
           var1_num_gold=int(gold_relation2[j][1][len(gold_label):])
           var2_num_test=int(test_relation2[i][2][len(test_label):])
           var2_num_gold=int(gold_relation2[j][2][len(gold_label):])
           candidate_match[var1_num_test].add(var1_num_gold)
           candidate_match[var2_num_test].add(var2_num_gold)
           cur_k1=(var1_num_test,var1_num_gold)
           cur_k2=(var2_num_test,var2_num_gold)
           if cur_k2!=cur_k1:
              if cur_k1 in weight_dict:
                 if cur_k2 in weight_dict[cur_k1]:
                    weight_dict[cur_k1][cur_k2]+=1
                 else:
                    weight_dict[cur_k1][cur_k2]=1
              else:
                 weight_dict[cur_k1]={}
                 weight_dict[cur_k1][-1]=0
                 weight_dict[cur_k1][cur_k2]=1
              if cur_k2 in weight_dict:
                 if cur_k1 in weight_dict[cur_k2]:
                    weight_dict[cur_k2][cur_k1]+=1
                 else:
                      weight_dict[cur_k2][cur_k1]=1
              else:
                 weight_dict[cur_k2]={}
                 weight_dict[cur_k2][-1]=0
                 weight_dict[cur_k2][cur_k1]=1
           else:
              #cycle
              if cur_k1 in weight_dict:
                 weight_dict[cur_k1][-1]+=1
              else:
                 weight_dict[cur_k1]={}
                 weight_dict[cur_k1][-1]=1
    return (candidate_match, weight_dict) 

def init_match(candidate_match,test_instance,gold_instance):
    """Initialize match based on the word match
       Args: 
           candidate_match: candidate variable match list
           test_instance: test instance
           gold_instance: gold instance
        Returns:
           intialized match result"""
    random.seed()
    matched_dict={}
    result=[]
    no_word_match=[]
    for i,c in enumerate(candidate_match):
      c2=list(c)
      if len(c2)==0:
         result.append(-1)
         continue
      #word in the test instance
      test_word=test_instance[i][2]
      for j,m_id in enumerate(c2):
        gold_word=gold_instance[int(m_id)][2]
        if test_word==gold_word:
           if int(m_id) not in matched_dict:
              result.append(int(m_id))
              matched_dict[int(m_id)]=1
              break
           #   found=True
      if len(result)==i:
         no_word_match.append(i)
         result.append(-1)
    for i in no_word_match:
        c2=list(candidate_match[i])
        found=False
        while len(c2)!=1:
          rid=random.randint(0,len(c2)-1)
          if c2[rid] in matched_dict:
             c2.pop(rid)
#        cur_rid=0
#        while cur_rid<len(c2):
#          #rid=random.randint(0,len(c2)-1)
#          if c2[cur_rid] in matched_dict:
#             cur_rid+=1
#             continue
          else:
             matched_dict[c2[rid]]=1
             result[i]=c2[rid]
        #     matched_dict[c2[cur_rid]]=1
        #     result[i]=c2[cur_rid]
             found=True
             break
        if not found:
           if c2[0] not in matched_dict:
              result[i]=c2[0]
              matched_dict[c2[0]]=1
      ##    result[i]=-1
    #       if c2[0] not in matched_dict:
    #          result[i]=c2[0]
    #          matched_dict[c2[0]]=1
    return result
        
          
           
def get_random_sol(candidate):
    """
    Generate a random variable mapping.
    Args:
        candidate:a list of set and each set contains the candidate match of a test instance
    """
    random.seed()
    matched_dict={}
    result=[]
    for c in candidate:
      c2=list(c)
      found=False
      if len(c2)==0:
         result.append(-1)
         continue
      while len(c2)!=1:
        rid=random.randint(0,len(c2)-1)
        if c2[rid] in matched_dict:
           c2.pop(rid)
        else:
           matched_dict[c2[rid]]=1
           result.append(c2[rid])
           found=True
           break
      if not found:
        if c2[0] not in matched_dict:
           result.append(c2[0])
           matched_dict[c2[0]]=1
        else:
           result.append(-1)
    return result

 
def compute_match(match,weight_dict):
    """Given a variable match, compute match number based on weight_dict. 
       Args:
           match: a list of number in gold set, len(match)= number of test instance
       Returns: 
           matching triple number
       Complexity: O(m*n) , m is the length of test instance, n is the length of gold instance"""
    #remember matching number of the previous matching we investigated
    if tuple(match) in match_num_dict:
       return match_num_dict[tuple(match)]
    match_num=0
    for i,m in enumerate(match):
      if m==-1:
         continue
      cur_m=(i,m)
      if cur_m not in weight_dict:
         continue
      match_num+=weight_dict[cur_m][-1]
      for k in weight_dict[cur_m]:
        if k==-1:
           continue
        if k[0]<i:
           continue
        elif match[k[0]]==k[1]:
           match_num+=weight_dict[cur_m][k]
    match_num_dict[tuple(match)]=match_num
    return match_num  

def move_gain(match,i,m,nm,weight_dict,match_num):
    """Compute the triple match number gain by the move operation
       Args:
           match: current match list
           i: the remapped source variable
           m: the original id
           nm: new mapped id
           weight_dict: weight dictionary
           match_num: the original matching number
        Returns:
           the gain number (might be negative)"""
    cur_m=(i,nm)
    old_m=(i,m)
    new_match=match[:]
    new_match[i]=nm
    if tuple(new_match) in match_num_dict:
       return match_num_dict[tuple(new_match)]-match_num
    gain=0
    if cur_m in weight_dict:
       gain+=weight_dict[cur_m][-1]
       for k in weight_dict[cur_m]:
         if k==-1:
           continue
         elif match[k[0]]==k[1]:
           gain+=weight_dict[cur_m][k]
    if old_m in weight_dict:
       gain-=weight_dict[old_m][-1]
       for k in weight_dict[old_m]:
         if k==-1:
           continue
         elif match[k[0]]==k[1]:
           gain-=weight_dict[old_m][k]
    match_num_dict[tuple(new_match)]=match_num+gain
    return gain

    
def swap_gain(match,i,m,j,m2,weight_dict,match_num):
    """Compute the triple match number gain by the swap operation
       Args:
           match: current match list
           i: the position 1
           m: the original mapped variable of i
           j: the position 2
           m2: the original mapped variable of j
           weight_dict: weight dictionary
           match_num: the original matching number
        Returns:
           the gain number (might be negative)"""
    new_match=match[:]
    new_match[i]=m2
    new_match[j]=m
    gain=0
    cur_m=(i,m2)
    cur_m2=(j,m)
    old_m=(i,m)
    old_m2=(j,m2)
    if cur_m in weight_dict:
       gain+=weight_dict[cur_m][-1]
       if cur_m2 in weight_dict[cur_m]:
          gain+=weight_dict[cur_m][cur_m2]
       for k in weight_dict[cur_m]:
         if k==-1:
           continue
         elif k[0]==j:
           continue
         elif match[k[0]]==k[1]:
           gain+=weight_dict[cur_m][k]
    if cur_m2 in weight_dict:
       gain+=weight_dict[cur_m2][-1]    
       for k in weight_dict[cur_m2]:
         if k==-1:
           continue
         elif k[0]==i:
           continue
         elif match[k[0]]==k[1]:
           gain+=weight_dict[cur_m2][k]
    if old_m in weight_dict:
       gain-=weight_dict[old_m][-1]    
       if old_m2 in weight_dict[old_m]:
          gain-=weight_dict[old_m][old_m2]
       for k in weight_dict[old_m]:
         if k==-1:
           continue
         elif k[0]==j:
           continue
         elif match[k[0]]==k[1]:
           gain-=weight_dict[old_m][k]
    if old_m2 in weight_dict:
       gain-=weight_dict[old_m2][-1]    
       for k in weight_dict[old_m2]:
         if k==-1:
           continue
         elif k[0]==i:
           continue
         elif match[k[0]]==k[1]:
           gain-=weight_dict[old_m2][k]
    match_num_dict[tuple(new_match)]=match_num+gain
    return gain

def get_best_gain(match,candidate_match,weight_dict,gold_len,start_match_num):
    """ hill-climbing method to return the best gain swap/move can get
      Args:
          match: the initial variable mapping
          candidate_match: the match candidates list
          weight_dict: the weight dictionary
          gold_len: the number of the variables in file 2
          start_match_num: the initial match number
      Returns:
          the best gain we can get via swap/move operation"""
    largest_gain=0
    largest_match_num=0
    swap=True  #True: using swap False: using move
    change_list=[]
    #unmatched gold number
    unmatched_gold=set(range(0,gold_len))
    #O(gold_len)
    for m in match:
      if m in unmatched_gold:
         unmatched_gold.remove(m)
    unmatch_list=list(unmatched_gold)
    for i,m in enumerate(match):
      #remap i
      for nm in unmatch_list:
        if nm in candidate_match[i]:
           #(i,m) -> (i,nm)    
           gain=move_gain(match,i,m,nm,weight_dict,start_match_num)
           if verbose:
              new_match=match[:]
              new_match[i]=nm
              new_m_num=compute_match(new_match,weight_dict)      
              if new_m_num!=start_match_num+gain:
                 print >> sys.stderr, match, new_match
                 print >> sys.stderr, "Inconsistency in computing: move gain", start_match_num, gain, new_m_num
           if gain>largest_gain:
              largest_gain=gain
              change_list=[i,nm]
              swap=False
              largest_match_num=start_match_num+gain
    for i,m in enumerate(match):
      for j,m2 in enumerate(match):
        #swap i
        if i==j:
           continue
        new_match=match[:]
        new_match[i]=m2
        new_match[j]=m
        sw_gain=swap_gain(match,i,m,j,m2,weight_dict,start_match_num)
        if verbose:
           new_match=match[:]
           new_match[i]=m2
           new_match[j]=m
           new_m_num=compute_match(new_match,weight_dict)     
           if new_m_num!=start_match_num+sw_gain:
              print >> sys.stderr, match, new_match
              print >> sys.stderr, "Inconsistency in computing: swap gain", start_match_num, sw_gain, new_m_num
        if sw_gain>largest_gain:
           largest_gain=sw_gain
           change_list=[i,j]
           swap=True
    cur_match=match[:]
    largest_match_num=start_match_num+largest_gain
    if change_list!=[]:
       if swap:
          temp=cur_match[change_list[0]]
          cur_match[change_list[0]]=cur_match[change_list[1]]
          cur_match[change_list[1]]=temp
         # print >> sys.stderr,"swap gain"
       else:
          cur_match[change_list[0]]=change_list[1]
         # print >> sys.stderr,"move gain"
    return (largest_match_num,cur_match)
          
    
    

def get_fh(test_instance,test_relation1,test_relation2,gold_instance,gold_relation1,gold_relation2,test_label,gold_label):
    """Get the f-score given two sets of triples
       Args:
           iter_num: iteration number of heuristic search
           test_instance: instance triples of AMR 1
           test_relation1: relation triples of AMR 1 (one-variable)
           test_relation2: relation triples of AMR 2 (two-variable)
           gold_instance: instance triples of AMR 2
           gold_relation1: relation triples of AMR 2 (one-variable)
           gold_relation2: relation triples of AMR 2 (two-variable)
           test_label: prefix label for AMRe 1
           gold_label: prefix label for AMR 2
        Returns:
           best_match: the variable mapping which results in the best matching triple number
           best_match_num: the highest matching number
          """ 
    #compute candidate pool
    (candidate_match,weight_dict)=compute_pool(test_instance,test_relation1,test_relation2,gold_instance,gold_relation1,gold_relation2,test_label,gold_label)
    best_match_num=0
    best_match=[-1]*len(test_instance)
    for i in range(0,iter_num):
      if verbose:
         print >> sys.stderr,"Iteration",i
      if i==0:
         #smart initialization
         start_match=init_match(candidate_match,test_instance,gold_instance)
      else:
         #random initialization
         start_match=get_random_sol(candidate_match)
      #first match_num, and store the match in memory
      match_num=compute_match(start_match,weight_dict)
     # match_num_dict[tuple(start_match)]=match_num 
      if verbose:
         print >> sys.stderr, "starting point match num:",match_num
         print >> sys.stderr,"start match",start_match
      #hill-climbing
      (largest_match_num,cur_match)=get_best_gain(start_match,candidate_match,weight_dict,len(gold_instance),match_num)
      if verbose:
         print >> sys.stderr, "Largest match number after the hill-climbing", largest_match_num
     # match_num=largest_match_num
      #hill-climbing until there will be no gain if we generate a new variable mapping
      while largest_match_num>match_num:
         match_num=largest_match_num
         (largest_match_num,cur_match)=get_best_gain(cur_match,candidate_match,weight_dict,len(gold_instance),match_num)
         if verbose:
            print >> sys.stderr, "Largest match number after the hill-climbing", largest_match_num
      if match_num>best_match_num:
         best_match=cur_match[:]
         best_match_num=match_num
    return (best_match,best_match_num)  
       
#help of inst_list: record a0 location in the test_instance ...
def print_alignment(match,test_instance,gold_instance,flip=False):
    """ print the alignment based on a match
    Args:
        match: current match, denoted by a list
        test_instance: instances of AMR 1
        gold_instance: instances of AMR 2
        filp: filp the test/gold or not"""
    result=[]
    for i,m in enumerate(match):
      if m==-1:
         if not flip:
            result.append(test_instance[i][1]+"("+test_instance[i][2]+")"+"-Null")
         else:
            result.append("Null-"+test_instance[i][1]+"("+test_instance[i][2]+")")
      else:
         if not flip:
            result.append(test_instance[i][1]+"("+test_instance[i][2]+")"+"-"+gold_instance[m][1]+"("+gold_instance[m][2]+")")
         else:
            result.append(gold_instance[m][1]+"("+gold_instance[m][2]+")"+"-"+test_instance[i][1]+"("+test_instance[i][2]+")")
    return " ".join(result)
            
def compute_f(match_num,test_num,gold_num):
    """ Compute the f-score based on the matching triple number, triple number of the AMR set 1, triple number of AMR set 2
        Args: 
           match_num: matching triple number
           test_num:  triple number of AMR 1
           gold_num:  triple number of AMR 2
        Returns:
           precision: match_num/test_num
           recall: match_num/gold_num
           f_score: 2*precision*recall/(precision+recall)"""
    if test_num==0 or gold_num==0:
       return (0.00,0.00,0.00)
    precision=float(match_num)/float(test_num)
    recall=float(match_num)/float(gold_num)
    if (precision+recall)!=0:
       f_score=2*precision*recall/(precision+recall)
       if verbose:
          print >> sys.stderr, "F-score:",f_score
       return (precision,recall,f_score)
    else:
       if verbose:
          print >> sys.stderr, "F-score:","0.0"
       return (precision,recall,0.00)

def main(args):
  """Main function of the smatch calculation program"""
  global verbose
  global iter_num
  global single_score
  global pr_flag
  global match_num_dict
  #set the restart number
  iter_num=args.r+1
  verbose=False
  if args.ms:
     single_score=False
  if args.v:
     verbose=True
  if args.pr:
     pr_flag=True 
  total_match_num=0
  total_test_num=0
  total_gold_num=0   
  sent_num=1
  while True:
     cur_amr1=get_amr_line(args.f[0])
     cur_amr2=get_amr_line(args.f[1]) 
     if cur_amr1=="" and cur_amr2=="":
       break
     if(cur_amr1==""):
       print >> sys.stderr, "Error: File 1 has less AMRs than file 2"
       print >> sys.stderr, "Ignoring remaining AMRs"
       break
      # print >> sys.stderr, "AMR 1 is empty"
       #continue
     if(cur_amr2==""):
       print >> sys.stderr, "Error: File 2 has less AMRs than file 1"
       print >> sys.stderr, "Ignoring remaining AMRs"
       break
      # print >> sys.stderr, "AMR 2 is empty" 
      # continue
     amr1=amr.AMR.parse_AMR_line(cur_amr1)
     amr2=amr.AMR.parse_AMR_line(cur_amr2)
     test_label="a"
     gold_label="b"
     amr1.rename_node(test_label)
     amr2.rename_node(gold_label)
     (test_inst,test_rel1,test_rel2)=amr1.get_triples2()
     (gold_inst,gold_rel1,gold_rel2)=amr2.get_triples2()
     if verbose:
       print "AMR pair",sent_num
       print >> sys.stderr,"Instance triples of AMR 1:",len(test_inst)
       print >> sys.stderr,test_inst
    #   print >> sys.stderr,"Relation triples of AMR 1:",len(test_rel)
       print >> sys.stderr,"Relation triples of AMR 1:",len(test_rel1)+len(test_rel2)
       print >>sys.stderr,test_rel1
       print >> sys.stderr,test_rel2
    #   print >> sys.stderr, test_rel
       print >> sys.stderr,"Instance triples of AMR 2:",len(gold_inst)
       print >> sys.stderr,gold_inst
    #   print >> sys.stderr,"Relation triples of file 2:",len(gold_rel)
       print >> sys.stderr,"Relation triples of AMR 2:",len(gold_rel1)+len(gold_rel2)
       #print >> sys.stderr,"Relation triples of file 2:",len(gold_rel1)+len(gold_rel2)
       print >> sys.stderr,gold_rel1
       print >> sys.stderr,gold_rel2
   #    print >> sys.stderr, gold_rel
     if len(test_inst)<len(gold_inst):
       (best_match,best_match_num)=get_fh(test_inst,test_rel1,test_rel2,gold_inst,gold_rel1,gold_rel2,test_label,gold_label)
       if verbose:
          print >> sys.stderr, "AMR pair ",sent_num
          print >> sys.stderr, "best match number",best_match_num
          print >> sys.stderr, "best match", best_match
          print >>sys.stderr,"Best Match:",print_alignment(best_match,test_inst,gold_inst)
     else:
       (best_match,best_match_num)=get_fh(gold_inst,gold_rel1,gold_rel2,test_inst,test_rel1,test_rel2,gold_label,test_label)
       if verbose:
          print >> sys.stderr, "Sent ",sent_num
          print >> sys.stderr, "best match number",best_match_num
          print >> sys.stderr, "best match", best_match
          print >>sys.stderr,"Best Match:",print_alignment(best_match,gold_inst,test_inst,True)
     if not single_score:
        (precision,recall,best_f_score)=compute_f(best_match_num,len(test_rel1)+len(test_inst)+len(test_rel2),len(gold_rel1)+len(gold_inst)+len(gold_rel2))
        print "Sentence",sent_num
	if pr_flag:
	   print "Precision: %.3f" % precision
	   print "Recall: %.3f" % recall
        print "Smatch score: %.3f" % best_f_score
     total_match_num+=best_match_num
     total_test_num+=len(test_rel1)+len(test_rel2)+len(test_inst)
     total_gold_num+=len(gold_rel1)+len(gold_rel2)+len(gold_inst)
     match_num_dict.clear()
     sent_num+=1# print "F-score:",best_f_score
  if verbose:
      print >> sys.stderr, "Total match num"
      print >> sys.stderr, total_match_num,total_test_num,total_gold_num
  if single_score:
      (precision,recall,best_f_score)=compute_f(total_match_num,total_test_num,total_gold_num)
      if pr_flag:
         print "Precision: %.3f" % precision
         print "Recall: %.3f" % recall
      print "Document F-score: %.3f" % best_f_score
  args.f[0].close()
  args.f[1].close()
 
if __name__=="__main__":
   parser=None
   args=None
   if sys.version_info[:2]!=(2,7):
      if sys.version_info[0]!=2 or sys.version_info[1]<5:
         print >> ERROR_LOG, "Smatch only supports python 2.5 or later"
	 exit(1)
      import optparse
      if len(sys.argv)==1:
         print >> ERROR_LOG,"No argument given. Please run smatch.py -h to see the argument descriptions."
	 exit(1)
      #requires version >=2.3!
      parser=build_arg_parser2();
      (args,opts)=parser.parse_args()
      #handling file errors
      #if not len(args.f)<2:
      #   print >> ERROR_LOG,"File number given is less than 2"
      #   exit(1)
      file_handle=[]
      if args.f==None:
         print >> ERROR_LOG, "smatch.py requires -f option to indicate two files containing AMR as input. Please run smatch.py -h to see the argument descriptions."
	 exit(1)
      if not os.path.exists(args.f[0]):
         print >> ERROR_LOG,"Given file",args.f[0], "does not exist"
         exit(1) 
      else:
         file_handle.append(open(args.f[0]))
      if not os.path.exists(args.f[1]):
         print >> ERROR_LOG,"Given file",args.f[1],"does not exist"
         exit(1)
      else:
         file_handle.append(open(args.f[1]))
      args.f=tuple(file_handle)
   else: #version 2.7
      import argparse
      parser=build_arg_parser()
      args=parser.parse_args()
   main(args)
