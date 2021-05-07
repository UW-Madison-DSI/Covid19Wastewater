#!/usr/bin/perl -w


use warnings;
use strict;
use Carp;
use English;
use Getopt::Long;


GetOptions (

            );

## see docs/LakeshoreSampling 
my %dormsSampled = (
    'Phillips' => 1,
    'Dejope'   => 1,
    'Bradley'  => 1,
    'Cole'     => 1,
    'Sullivan' => 1,
    'Sellery'  => 1,
    );

####
my @lakeShoreDormCols = ();
my $selleryCols;
my @orphanData;  ## hack to work-around excel formatting;


while(<>) {

    chomp;
    if (scalar @lakeShoreDormCols == 0) {
	### find columns corresponding to UW_DORMS
	next unless /^\s*Off campus/;
	my @dormList = split /\t/;
	foreach my $index (0..$#dormList) {
	    ## remove tailing spaces
	    (my $dorm = $dormList[$index]) =~ s/\s*$//;
	    next unless (exists $dormsSampled{$dorm});
	    ## [dorm, neg,pos]
	    my $dormIndices = [$dorm,$index,$index+1];
	    #print "$dorm\n";
	    
	    if ($dorm eq 'Sellery') {
		$selleryCols = $dormIndices;
	    }
	    else {
		push @lakeShoreDormCols,$dormIndices;
	    } ##if/else
	} ## foreach
	next;
    } ## if
    
    next if /^\s*Neg/;  ## skip additional header line;

    ## fix alignment of Date for first data row 
    my @testingData = split /\t/;
    if (/Date of Collection/) {
	@orphanData = @testingData;
	next;
    }
    else {
	if (scalar @orphanData != 0) {
	    ## assign date to data from previous line
	    my $date = $testingData[0];
	    @testingData = ($date,@orphanData[1..$#orphanData]);
	    @orphanData = ();  ## null out orphanData
	} ##
    } ## if/else
    
    my $date = $testingData[0];
    last if ($date =~ /Total/);
    ## print Sellery;
    my $selleryNeg = $testingData[$selleryCols->[1]];
    my $selleryPos = $testingData[$selleryCols->[2]];
    map {
	$_ = 0 if ($_ eq '.'); ## convert '.' to 0
	s/^\s*//;              ## leading spaces

    } ($selleryNeg,$selleryPos);
    print join("\t", $date, "UW_S",
	       $selleryNeg,
	       $selleryPos,
	), "\n";
    my $negatives = 0;
    my $positives = 0;
    foreach my $dormData (@lakeShoreDormCols) {
	my $neg = $testingData[$dormData->[1]];
	my $pos = $testingData[$dormData->[2]];
	## replace '.' with 0;
	map {$_ = 0 if ($_ eq '.')} ($neg,$pos);

	$negatives += $neg;
	$positives += $pos;
    }
    print join("\t", $date, "UW_D", $negatives, $positives), "\n";
}
__END__


