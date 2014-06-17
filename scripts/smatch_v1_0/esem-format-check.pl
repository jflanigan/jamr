#!/usr/bin/perl -w

# Author: Ulf Hermjakob

$html_filename = "";
$pprint_filename = "";
$n_errors = 0;
$n_warnings = 0;
$kevin_indentation_style = 0;

while (@ARGV) {
   $arg = shift @ARGV;
   if ($arg =~ /^-+html/) {
      $html_filename = shift @ARGV;
   } elsif ($arg =~ /^-+pprint/) {
      $pprint_filename = shift @ARGV;
   } elsif ($arg =~ /^-+kis/) {
      $kevin_indentation_style = 1;
   } elsif ($arg =~ /^-+(h|help)$/i) {
      &help();
      exit 1;
   } else {
      print STDERR "Ignoring unrecognized argument $arg\n";
   }
}

sub help {
   print STDERR "This program checks AMRs for well-formedness and reformats them (in terms of indentation).\n\n";
   print STDERR "Usage: esem-format-check.pl < amrs.txt > reformatted-amrs.txt\n";
   print STDERR "   errors and warnings written to STDERR\n\n";
   print STDERR "Script written by Ulf Hermjakob/ISI on February 9, 2012\n";
   print STDERR "Version 0.2 - February 10, 2012\n";
   exit 1;
}

sub initialize_amr {
   %ht = ();
   $ht{ROLES_AT_LEVEL}->{0} = 0;
   $level = 0;
   @role_path = ();
   $amr_start_line_number = 0;
   $status = "outside-AMR";
   $unexpected_new_amr = 0;
}

sub print_ht_struct {
   local(*ht, *OUT, $control, $level, $role_path_s) = @_;
   
   $control = "" unless defined($control);
   $level = 0 unless defined($level);
   $role_path_s = "" unless defined($role_path_s);
   my $indent = "";
   if ($kevin_indentation_style) {
      @roles = split(/\s+/, $role_path_s);
      $indent = "  ";
      foreach $i ((0 .. $#roles)) {
	 my $s = $roles[$i];
	 $s =~ s/./ /g;
         $indent .= "   $s";
      }
   } else {
      $indent = "   ";
      foreach $i ((1 .. $level)) {
         $indent .= "     ";
      }
   }
   if ($level == 0) {
      $amr_end_line_number = $line_number;
      $amr_end_line_number-- if $unexpected_new_amr;
      if ($control =~ /incomplete/) {
	 print OUT "# Incomplete AMR (lines $amr_start_line_number-$amr_end_line_number):\n";
      } else {
#	 print OUT "# AMR (lines $amr_start_line_number-$amr_end_line_number):\n";
      }
   }
   if (($var = $ht{S}->{$role_path_s}->{VAR})
    && ($concept = $ht{S}->{$role_path_s}->{CONCEPT})) {
       print OUT "($var \/ $concept";
       foreach $role (sort keys %{$ht{ROLES_AT_PATH}->{$role_path_s}}) {
	  $index_free_role = $role;
	  $index_free_role =~ s/\.\d+$//;
	  print OUT "\n$indent$index_free_role ";
	  my $new_role_path_s = ($role_path_s eq "") ? $role : "$role_path_s $role";
	  &print_ht_struct(*ht, *OUT, $control, ($level + 1), $new_role_path_s);
       }
       print OUT ")";
   } elsif ($var = $ht{S}->{$role_path_s}->{VAR}) {
       print OUT $var;
   } elsif ($value = $ht{S}->{$role_path_s}->{VALUE}) {
       print OUT $value;
   } else {
       print OUT "<***>";
   }
   print OUT "\n\n" if $level == 0;
}

&initialize_amr();

$line_number = 0;
while (<STDIN>) {
   $line_number++;
   $s = $_;
   $s =~ s/\*$//;
   last if $s =~ /^#\s*EXIT\s*$/;
   if ($s =~ /^#/) {
   #   print $s;
      next;
   }
   $s =~ s/\#.*$//;
   $s =~ s/\xE2\x80\x9C([ -~]+)\xE2\x80\x9D/"$1"/g;
   next unless $s =~ /\S/;

   # new AMR starting in first column following incomplete AMR
   if (($s =~ /^\([a-z] \/ /) && ($level > 0)) { # )
      print STDERR "*** Line $line_number - Incomplete AMR terminated by new AMR starting without indentation; ";
      $n_errors++;
      if ($level >= 2) {
	 print STDERR "$level missing close parentheses. ";
      } else {
	 print STDERR "missing close parentheses. ";
      }
      print STDERR "\n";
      $unexpected_new_amr = 1;
      &print_ht_struct(*ht, *STDOUT, "incomplete");
      $level = 0;
   }

   &initialize_amr() if $level == 0;

   while ($s =~ /\S/) {
      # (var / concept [...])
      if (($status =~ /^(outside-AMR|pre-value|unexpected)$/) && (($var, $concept, $rest) = ($s =~ /^\s*\(\s*([a-z]\d*)\s+\/\s+([a-zA-Z0-9][-a-zA-Z0-9]*|\*OR\*|"[^"]+"|\|[^\|]+\|)(|\).*|\s.*)$/))) {
	 $role_path_s = join(" ", @role_path);
	 $ht{S}->{$role_path_s}->{VAR} = $var;
	 $ht{S}->{$role_path_s}->{CONCEPT} = $concept;
	 if ($ht{VAR_COUNT}->{$var}
	  && ($old_concept = $ht{VAR_CONCEPT}->{$var})) {
	    $old_line_number = $ht{VAR_DEF_LINE}->{$var};
	    print STDERR "*** Line $line_number - Variable \"$var\", defined as \"$old_concept\" in line $old_line_number, redefined as \"$concept\"\n";
            $n_errors++;
	 } else {
	    $ht{VAR_CONCEPT}->{$var} = $concept;
	    $ht{VAR_DEF_LINE}->{$var} = $line_number;
	 }
	 $ht{VAR_COUNT}->{$var} = ($ht{VAR_COUNT}->{$var} || 0) + 1;
	 $ht{VAR}->{$var}->{$role_path_s} = 1;
	 $status = "pre-role";
	 # print STDERR "  PATH($level): $role_path_s VAR: $var CONCEPT: $concept\n";
	 $amr_start_line_number = $line_number if $level == 0;
	 $level++;
	 $ht{ROLES_AT_LEVEL}->{$level} = 0;
	 $s = $rest; 
	 # (
      # var
      } elsif (($status eq "pre-value") && (($var, $rest) = ($s =~ /^\s*([a-z]\d*)(|\).*|\s.*)$/))) {
	 $role_path_s = join(" ", @role_path);
	 $ht{S}->{$role_path_s}->{VAR} = $var;
	 unless ($ht{VAR_COUNT}->{$var}) {
	    print STDERR "*   Line $line_number - Warning: Use of previously undefined variable \"$var\"\n";
            $n_warnings++;
	 }
	 $ht{VAR_COUNT}->{$var} = ($ht{VAR_COUNT}->{$var} || 0) + 1;
	 $ht{VAR}->{$var}->{$role_path_s} = 1;
	 $status = "pre-role";
	 pop(@role_path) if ($#role_path + 1) >= $level;
	 $s = $rest;
	 # (
      # expected atomic-value
      } elsif (($status eq "pre-value") && (($value, $rest) = ($s =~ /^\s*("[^"]+"|-?\d+(?:\.\d+)?|-|amr-unknown|interrogative|[A-Z][-A-Z]+)(|\).*|\s.*)$/))) {
	 $role_path_s = join(" ", @role_path);
	 $ht{S}->{$role_path_s}->{VALUE} = $value;
	 $status = "pre-role";
	 # print STDERR "  PATH($level): $role_path_s VALUE: $value\n";
	 pop(@role_path) if ($#role_path + 1) >= $level;
	 $s = $rest;
	 # (
      # well-formed close parenthesis
      } elsif (($status =~ /^(pre-role|unexpected)$/) && ($level > 0) && ($s =~ /^\s*\)/)) {
	 pop(@role_path) if ($#role_path + 1) >= $level;
	 $level--;
	 pop(@role_path) if ($#role_path + 1) >= $level;
	 $status = ($level == 0) ? "outside-AMR" : "pre-role";
	 # (
	 $s =~ s/^\s*\)//;
	 # (
      # unexpected close parenthesis
      } elsif ((($status =~ /^(outside-AMR)$/) || ($level == 0)) && ($s =~ /^\s*\)/)) {
	 print STDERR "*** Line $line_number - Non-matching close parenthesis.\n";
         $n_errors++;
	 $level = 0;
	 @role_path = ();
	 $status = "outside-AMR";
	 # (
	 $s =~ s/^\s*\)//;
	 # (
      # :role
      } elsif (($status =~ /^(pre-role|unexpected)$/) && (($role, $rest) = ($s =~ /^\s*((?::[a-z][-a-z0-9]*|:ARG\d+|:ARGM-[A-Z]{3,3})(?:-of|-like)?)(|\)|\s.*)$/))) {
	 $ht{ROLES_AT_LEVEL}->{$level} = ($ht{ROLES_AT_LEVEL}->{$level} || 0) + 1;
	 # print STDERR "  Roles at level $level: " . $ht{ROLES_AT_LEVEL}->{$level} . "\n";
	 $i = 1;
	 $parent_role_path_s = join(" ", @role_path);
	 push(@role_path, $role);
	 $role_path_s = join(" ", @role_path);
	 $indexed_role = $role;
	 while ($ht{S}->{$role_path_s}) {
	   $i++;
	   pop(@role_path);
	   $indexed_role = "$role.$i";
	   push(@role_path, $indexed_role);
	   $role_path_s = join(" ", @role_path);
	 }
	 $ht{ROLES_AT_PATH}->{$parent_role_path_s}->{$indexed_role} = 1;
	 $status = "pre-value";
	 $s = $rest;
      # unexpected token
      } elsif (($token, $rest) = ($s =~ /^\s*(\S|\S.*?[^ :]|\S.*?\S:)\s*((?: :|\(|\)).*|)$/)) {
	 $token =~ s/\s*$//;
	 if ($token =~ /\s/) {
	    print STDERR "*** Line $line_number - Ignoring unexpected tokens: $token\n";
	 } else {
	    print STDERR "*** Line $line_number - Ignoring unexpected token: $token\n";
	 }
         if ($token =~ /^:/) {
	    $n_warnings++;
	 } else {
            $n_errors++;
	 }
	 $status = "unexpected";
	 $s = $rest;
	 # (
      } else {
	 print STDERR "*** Line $line_number - Unexpected text at status $status: $s\n";
         $n_errors++;
         $s = "";
      }
   }
   if ($level == 0) {
      &print_ht_struct(*ht, *STDOUT);
      $status = "outside-AMR";
   }
}

if ($level > 0) {
   if ($level >= 2) {
      print STDERR "*** Line $line_number - $level missing close parentheses at end of file.\n";
      $n_errors++;
   } elsif ($level > 0) {
      print STDERR "*** Line $line_number - Missing close parenthesis at end of file.\n";
      $n_errors++;
   }
   &print_ht_struct(*ht, *STDOUT, "incomplete");
}

if ($n_errors || $n_warnings) {
   $message = "";
   $message .= "$n_errors errors" if $n_errors >= 2;
   $message .= "$n_errors error" if $n_errors == 1;
   $message .= " and " if $n_errors && $n_warnings;
   $message .= "$n_warnings warnings" if $n_warnings >= 2;
   $message .= "$n_warnings warning" if $n_warnings == 1;
   $message .= " in $line_number lines.\n";
   print STDERR $message;
}

exit ($n_errors) ? 1 : 0;

