#!/jo/bin/env perl
#edited by Will
#requies commandline args for titlefile and garbage
#HAS ISSUE WITH NUMBERING (not major)
use strict;
use warnings;
use diagnostics;

# program to parse scripts into scenes
# read title file produced by recurse_dir.pl
#my $titlefile = 'J:\work\garbage.txt';
open(TITLEFILE, "<$ARGV[0]") or die("Error: cannot open file $ARGV[0]\n");

# output to garbage if end of file html krap
#my $garbage = 'X:\projects\center\grants\TRO\data\garbage\garbage.txt';	
open(GARBAGE, ">$ARGV[1]") or die("Error: cannot open file $ARGV[1]\n");

my ($line1, $line2);
my $exten = ".txt";
my $counter = 0;
my $space = "_";

foreach $line1 (<TITLEFILE>)
{
		print "$line1\n";
		# open each file for read from title file names
		chomp($line1);
		
		if( $line1 =~ /(.+)\.txt$/ )
		{
			my $infile = $line1;
			#print $infile;
			my $titletext = $1;
			open(INFILE, "<$infile") or die("Error: cannot open file $infile\n");
			
			# base name for separate scene files/documents
			#expect sub directory Parsed to exist
			my $scenefile = 'Parsed/';
			open(SCENEFILE, ">$scenefile$titletext$space$counter$exten") or die("Error: cannot open file '$scenefile$titletext$space$counter$exten'\n");
		
			while($line2 = <INFILE>)
			{
				# get rid of garbage at the end
				if($line2 =~ m/(^\s{5,}the end\s+$|^\s+the end\s{5,}$|^the end\s{5,}\r|^\s{5,}the end\r|^\s{5,}THE END\s+$|^\s+THE END\s{5,}$|^THE END\s{5,}\r|^\s{5,}THE END\r|(.*?)THE END(.*?))/)
				{
					print SCENEFILE $line2;
					while($line2 = <INFILE>)
					{		
						print GARBAGE $line2;
					}
				}
				else
				{
					# ignore page numbers - could be tricky if just scene numbers!!!!
					if($line2 !~ m/(^\d{1,3}\.\s+\r$|^\s+\d{1,3}\.\r$)/) 
					{	
						#regex scene parser
						#next $line if (m/^$/); # ignore null lines
						#if($line2 =~ m/^(INT.  |INT.(.+)-|INT (.+)-|INT.(.*?)DAY|INT.(.*?)NIGHT|INT.(.*?)AFTERNOON|INT.(.*?)EVENING|INT.(.*?)DAWN|INT.(.*?)DUSK|INT.(.*?)SAME TIME|EXT.  |EXT.(.+)-|EXT (.+)-|EXT.(.*?)DAY|EXT.(.*?)NIGHT|EXT.(.*?)AFTERNOON|EXT.(.*?)EVENING|EXT.(.*?)DAWN|EXT.(.*?)DUSK|EXT.(.*?)SAME TIME|NTERIOR(.*?)DAY|INTERIOR(.*?)NIGHT|INTERIOR(.*?)AFTERNOON|INTERIOR(.*?)EVENING|INTERIOR(.*?)DAWN|INTERIOR(.*?)DUSK|INTERIOR(.*?)SAME TIME|EXTERIOR(.*?)DAY|EXTERIOR(.*?)NIGHT|EXTERIOR(.*?)AFTERNOON|EXTERIOR(.*?)EVENING|EXTERIOR(.*?)DAWN|EXTERIOR(.*?)DUSK|EXTERIOR(.*?)SAME TIME|SCENE:)/gi)
						if($line2 =~ m/(INT.  |INT.(.+)-|INT (.+)-|INT.(.*?)DAY|INT.(.*?)NIGHT|INT.(.*?)AFTERNOON|INT.(.*?)EVENING|INT.(.*?)DAWN|INT.(.*?)DUSK|INT.(.*?)SAME TIME|EXT.  |EXT.(.+)-|EXT (.+)-|EXT.(.*?)DAY|EXT.(.*?)NIGHT|EXT.(.*?)AFTERNOON|EXT.(.*?)EVENING|EXT.(.*?)DAWN|EXT.(.*?)DUSK|EXT.(.*?)SAME TIME|NTERIOR(.*?)DAY|INTERIOR(.*?)NIGHT|INTERIOR(.*?)AFTERNOON|INTERIOR(.*?)EVENING|INTERIOR(.*?)DAWN|INTERIOR(.*?)DUSK|INTERIOR(.*?)SAME TIME|EXTERIOR(.*?)DAY|EXTERIOR(.*?)NIGHT|EXTERIOR(.*?)AFTERNOON|EXTERIOR(.*?)EVENING|EXTERIOR(.*?)DAWN|EXTERIOR(.*?)DUSK|EXTERIOR(.*?)SAME TIME|SCENE:)/gi)
						{
							$counter = $counter + 1;
							close(SCENEFILE);
							open(SCENEFILE, ">$scenefile$titletext$space$counter$exten") or die("Error: cannot open file '$scenefile$titletext$space$counter$exten'\n");
						}
						
						$line2 =~ tr/[A-Z]/[a-z]/;
						print SCENEFILE $line2;
					}
				}
			}
			close(SCENEFILE);
			close(INFILE);
		}
		$counter = 0;
}

close(GARBAGE);

exit;
