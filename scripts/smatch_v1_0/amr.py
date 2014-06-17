#!/usr/bin/env python

import sys
from collections import defaultdict

class AMR(object):
  def __init__(self,var_list=None,var_value_list=None,link_list=None,const_link_list=None):
    if var_list is None:
       self.nodes=[]  #AMR variables 
       self.root=None
    else:
       self.nodes=var_list[:]
       if len(var_list)!=0:
          self.root=var_list[0]
       else:
          self.root=None
    if var_value_list is None:
       self.var_values=[]
    else:
       self.var_values=var_value_list[:]
    if link_list is None: 
       self.links=[]  #connections between instances  #adjacent list representation
    else:
       self.links=link_list[:]
    if const_link_list is None:
       self.const_links=[]
    else:
       self.const_links=const_link_list[:]
    

  def add_node(node_value):
    self.nodes.append(node_value)

  def rename_node(self,prefix):
    var_map_dict={}
    for i in range(0,len(self.nodes)):
      var_map_dict[self.nodes[i]]=prefix+str(i)
    for i,v in enumerate(self.nodes):
      self.nodes[i]=var_map_dict[v]
    for i,d in enumerate(self.links):
      new_dict={}
      for k,v in d.items():
        new_dict[var_map_dict[k]]=v
      self.links[i]=new_dict
  
  def get_triples(self):
      """Get the triples in two list: instance_triple, relation_triple"""
      instance_triple=[]
      relation_triple=[]
      for i in range(len(self.nodes)):
        instance_triple.append(("instance",self.nodes[i],self.var_values[i]))
        for k,v in self.links[i].items():
            relation_triple.append((v,self.nodes[i],k))
        for k2,v2 in self.const_links[i].items():
            relation_triple.append((k2,self.nodes[i],v2))
      return (instance_triple,relation_triple)    
  
  def get_triples2(self):
      """Get the triples in three lists: instance_triple, relation (two variables) triple, and relation (one variable) triple"""
      instance_triple=[]
      relation_triple1=[]
      relation_triple2=[]
      for i in range(len(self.nodes)):
        instance_triple.append(("instance",self.nodes[i],self.var_values[i]))
        for k,v in self.links[i].items():
            relation_triple2.append((v,self.nodes[i],k))
        for k2,v2 in self.const_links[i].items():
            relation_triple1.append((k2,self.nodes[i],v2))
      return (instance_triple,relation_triple1,relation_triple2)    
 
  def __str__(self):
      """Output AMR string"""
      for i in range(len(self.nodes)):
        print "Variable", i, self.nodes[i]
        print "Dependencies:"
        for k,v in self.links[i].items():
          print "Variable", k, " via ",v
        for k2,v2 in self.const_links[i].items():
          print "Attribute:",k2, "value",v2

  def __repr__(self):
      return self.__str__()

  def out_amr(self):
      self.__str__()    

  @staticmethod
  def parse_AMR_line(line):
    state=-1 #significant symbol just encountered: 1 for (, 2 for :, 3 for /
    stack=[] #variable stack
    cur_charseq=[] #current processing char sequence
    var_dict={} #key: var name value: var value
    var_list=[] #variable name list (order: occurence of the variable
    var_attr_dict1=defaultdict(list) #key: var name:  value: list of (attribute name, other variable)
    var_attr_dict2=defaultdict(list) #key:var name, value: list of (attribute name, const value)
    cur_attr_name="" #current attribute name
    attr_list=[] #each entry is an attr dict
    in_quote=False
    for i,c in enumerate(line.strip()):
      if c==" ": 
         if state==2:
            cur_charseq.append(c)
         continue
      if c=="\"":
         if in_quote:
            in_quote=False
         else:
            in_quote=True
      if c=="(":
         if in_quote:
            continue
         if state==2:
            if cur_attr_name!="":
               print >> sys.stderr, "Format error when processing ",line[0:i+1] 
               return None
            cur_attr_name="".join(cur_charseq).strip()
            cur_charseq[:]=[]
         state=1
      elif c==":":
         if in_quote:
            continue
         if state==3: #(...:
            var_value="".join(cur_charseq) 
            cur_charseq[:]=[]
            cur_var_name=stack[-1]
            var_dict[cur_var_name]=var_value
         elif state==2: #: ...:
            temp_attr_value="".join(cur_charseq) 
            cur_charseq[:]=[]
            parts=temp_attr_value.split()
            if len(parts)<2:
               print >> sys.stderr, "Error in processing",line[0:i+1]
               return None
            attr_name=parts[0].strip()
            attr_value=parts[1].strip()
            if len(stack)==0:
               print >> sys.stderr, "Error in processing",line[:i],attr_name,attr_value
               return None
            if attr_value not in var_dict:
               var_attr_dict2[stack[-1]].append((attr_name,attr_value))
               #print stack[-1],attr_name,attr_value
            else:
    #           print stack[-2],attr_name,attr_value
               var_attr_dict1[stack[-1]].append((attr_name,attr_value))
            
            #   if attr_value[0]=="\"" and attr_value[-1]=="\"":
            #      attr_value=attr_value[1:-1]
           # attr_dict[attr_name]=attr_value
       #  elif state==4: #:)....:
       #     pass
         state=2
      elif c=="/":
         if in_quote:
            continue
         if state==1:
            variable_name="".join(cur_charseq)
            cur_charseq[:]=[]
            if variable_name in var_dict:
               print >> sys.stderr, "Duplicate variable ",variable_name, " in parsing AMR"
               return None
            stack.append(variable_name)
            var_list.append(variable_name)
      #      print variable_name,cur_attr_name
            if cur_attr_name!="":
               if not cur_attr_name.endswith("-of"):
                 # var_attr_dict1[stack[-2]][cur_attr_name]=variable_name
                  var_attr_dict1[stack[-2]].append((cur_attr_name, variable_name))
     #             print stack[-2]
               else:
                  #print variable_name,cur_attr_name,stack[-2]
                  var_attr_dict1[variable_name].append((cur_attr_name[:-3],stack[-2]))
                  #var_attr_dict1[variable_name][cur_attr_name[:-3]]=stack[-2]
               cur_attr_name=""
         else:
            #print state
            print >> sys.stderr, "Error in parsing AMR", line[0:i+1]
            return None
         state=3
      elif c==")": 
         if in_quote:
            continue
         #pop
         if len(stack)==0:
            print >> sys.stderr, "Unmatched parathesis at position", i, "in processing", line[0:i+1]
            return None
         if state==2:
            temp_attr_value="".join(cur_charseq) 
            cur_charseq[:]=[]
            parts=temp_attr_value.split()
            if len(parts)<2:
               print >> sys.stderr, "Error processing",line[:i+1],temp_attr_value
               return None 
            attr_name=parts[0].strip()
            attr_value=parts[1].strip()
            if cur_attr_name.endswith("-of"):
               var_attr_dict1[variable_name].append((cur_attr_name[:-3],stack[-2])) 
            elif attr_value not in var_dict:
               var_attr_dict2[stack[-1]].append((attr_name,attr_value))
               #print stack[-1],attr_name,attr_value
            else:
               var_attr_dict1[stack[-1]].append((attr_name,attr_value))
         elif state==3:
            var_value="".join(cur_charseq) 
            cur_charseq[:]=[]
            cur_var_name=stack[-1]
            var_dict[cur_var_name]=var_value
         stack.pop() 
       #  if cur_attr_name!="":
       #     var_attr_dict[cur_attr_name]= 
       #  var_name.append(cur_var_name)
       #  attr_list.append(attr_dict)
         cur_attr_name=""
         state=4 
      else:
         cur_charseq.append(c)
    #create var_list, link_list, attribute
    #keep original variable name.
    var_value_list=[]
    link_list=[]
    const_attr_list=[]
    for v in var_list:
      if v not in var_dict:
         print >> sys.stderr, "Error: variable value not found", v
         return None
      else:
         var_value_list.append(var_dict[v])
      link_dict={}
      const_dict={}
      if v in var_attr_dict1:
         for v1 in var_attr_dict1[v]:
           link_dict[v1[1]]=v1[0]
         #for k1,v1 in var_attr_dict1[v].items():
          # if v in var_list:
       #    link_dict[v1]=k1
         #  else:
         #     const_dict[v2]=k
    #     print v, var_attr_dict2[v]
      if v in var_attr_dict2:
         for v2 in var_attr_dict2[v]:
           if v2[1][0]=="\"" and v2[1][-1]=="\"":
              const_dict[v2[0]]=v2[1][1:-1]
           elif v2[1] in var_dict:
              link_dict[v2[1]]=v2[0]
           else:
              const_dict[v2[0]]=v2[1]
       #  for k2,v2 in var_attr_dict2[v].items():
       #    if v2[0]=="\"" and v2[1]=="\"":
       #       const_dict[k2]=v2[1:-1]
        #   elif v2 in var_dict:
      #        print "test",v,v2,k2
        #      link_dict[v2]=k2
        #   else:
        #      const_dict[k2]=v2
      link_list.append(link_dict)
      const_attr_list.append(const_dict)
      const_attr_list[0]["TOP"]=var_value_list[0] # add TOP relation
    result_amr=AMR(var_list,var_value_list,link_list,const_attr_list)
    #print result_amr.links
    return result_amr
