#!/usr/bin/perl -w
use strict;
my $bresto_enable = 1;
die "bresto is not enabled" if (not $bresto_enable);

=head1 LICENSE

   Bweb - A Bacula web interface
   Bacula® - The Network Backup Solution

   Copyright (C) 2000-2010 Free Software Foundation Europe e.V.

   The main author of Bweb is Eric Bollengier.
   The main author of Bacula is Kern Sibbald, with contributions from
   many others, a complete list can be found in the file AUTHORS.
   This program is Free Software; you can redistribute it and/or
   modify it under the terms of version three of the GNU Affero General Public
   License as published by the Free Software Foundation and included
   in the file LICENSE.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   Bacula® is a registered trademark of Kern Sibbald.
   The licensor of Bacula is the Free Software Foundation Europe
   (FSFE), Fiduciary Program, Sumatrastrasse 25, 8006 Zürich,
   Switzerland, email:ftf@fsfeurope.org.

=cut

use Bweb;

package Bvfs;
use base qw/Bweb/;

sub get_root
{
    my ($self) = @_;
    return $self->get_pathid('');
}

# change the current directory
sub ch_dir
{
    my ($self, $pathid) = @_;
    $self->{cwdid} = $pathid;
}

# return the current PWD
sub pwd
{
    my ($self) = @_;
    return $self->get_path($self->{cwdid});
}

# get the Path from a PathId
sub get_path
{
    my ($self, $pathid) = @_;
    $self->debug("Call with pathid = $pathid");
    my $query =	"SELECT Path FROM Path WHERE PathId = ?";
    my $sth = $self->dbh_prepare($query);
    $sth->execute($pathid);
    my $result = $sth->fetchrow_arrayref();
    $sth->finish();
    return $result->[0];
}

# we are working with these jobids
sub set_curjobids
{
    my ($self, @jobids) = @_;
    $self->{curjobids} = join(',', @jobids);
#    $self->update_brestore_table(@jobids);
}

# get the PathId from a Path
sub get_pathid
{
    my ($self, $dir) = @_;
    my $query =
	"SELECT PathId FROM Path WHERE Path = ?";
    my $sth = $self->dbh_prepare($query);
    $sth->execute($dir);
    my $result = $sth->fetchrow_arrayref();
    $sth->finish();

    return $result->[0];
}

sub set_limits
{
    my ($self, $offset, $limit) = @_;
    $self->{limit}  = $limit  || 100;
    $self->{offset} = $offset || 0;
}

sub set_pattern
{
    my ($self, $pattern) = @_;
    $self->{pattern} = $pattern;
}

# fill brestore_xxx tables for speedup
sub update_cache
{
    my ($self) = @_;
    my $b = $self->get_bconsole();
    $b->send_one_cmd(".bvfs_update" . $self->{bvfs_user});
}

sub update_brestore_table
{
    my ($self, @jobs) = @_;
    my $jobs = join(",", sort {$a <=> $b} @jobs);
    my $b = $self->get_bconsole();
    $b->send_one_cmd(".bvfs_update jobid=$jobs" . $self->{bvfs_user});
}

# list all files in a directory, accross curjobids
sub ls_files
{
    my ($self) = @_;

    return undef unless ($self->{curjobids});
    
    my $pathid = $self->{cwdid};
    my $jobclause = $self->{curjobids};
    my $filter ='';

    if ($self->{pattern}) {
        $filter = " pattern=\"$self->{pattern}\"";
    }
    my $b = $self->get_bconsole();
    my $ret = $b->send_one_cmd(".bvfs_lsfiles jobid=$jobclause " .
                               "pathid=$pathid " . $self->{bvfs_user} .
                               "limit=$self->{limit} offset=$self->{offset} " .
                               $filter);

    #   0        1          2       3     4       5
    # PathId, FilenameId, fileid, jobid, lstat, Name
    my @return_list;
    foreach my $line (@{$ret})
    {
        next unless ($line =~ /^\d+\t\d+/);
        chomp($line);
        my @row = split("\t", $line, 6);
	my $fid = $row[2];
        my $fnid = $row[1];
        my $name = $row[5];
        my $lstat = $row[4];
        my $jobid = $row[3] || 0;
        # We have to clean up this dirname ... we only want it's 'basename'
        my @return_array = ($fnid, $fid,$name,$lstat,$jobid);
        push @return_list,(\@return_array);
    }
#FilenameId, listfiles.id, Name, File.LStat, File.JobId

    return \@return_list;
}

# list all directories in a directory, accross curjobids
# return ($dirid,$dir_basename,$lstat,$jobid)
sub ls_dirs
{
    my ($self) = @_;

    return undef unless ($self->{curjobids});
    
    my $pathid = $self->{cwdid};
    my $jobclause = $self->{curjobids};
    my $filter ='';

    if ($self->{pattern}) {
        $filter = " pattern=\"$self->{pattern}\" ";
    }
    my $b = $self->get_bconsole();
    my $ret = $b->send_one_cmd(".bvfs_lsdir jobid=$jobclause pathid=$pathid " .
                               $self->{bvfs_user} .
                               "limit=$self->{limit} offset=$self->{offset} " .
                               $filter);

    #   0     1     2      3      4     5
    # PathId, 0, fileid, jobid, lstat, path
    my @return_list;
    my $prev_dir='';
    foreach my $line (@{$ret})
    {
        next unless ($line =~ /^\d+\t\d+/);
        chomp($line);
        my @row = split("\t", $line, 6);
	my $dirid = $row[0];
        my $dir = $row[5];
        my $lstat = $row[4];
        my $jobid = $row[3] || 0;
        next if ($self->{skipdot} && $dir =~ /^\.+$/);
        # We have to clean up this dirname ... we only want it's 'basename'
        my @return_array = ($dirid,$dir,'', $lstat,$jobid);
        push @return_list,(\@return_array);
        $prev_dir = $dirid;
    }

    return \@return_list;
}

# TODO : we want be able to restore files from a bad ended backup
# we have JobStatus IN ('T', 'A', 'E') and we must

# Data acces subs from here. Interaction with SGBD and caching

# This sub retrieves the list of jobs corresponding to the jobs selected in the
# GUI and stores them in @CurrentJobIds.
# date must be quoted
sub set_job_ids_for_date
{
    my ($self, $client, $date)=@_;

    if (!$client or !$date) {
	return ();
    }
    my $filter = $self->get_client_filter();
    # The algorithm : for a client, we get all the backups for each
    # fileset, in reverse order Then, for each fileset, we store the 'good'
    # incrementals and differentials until we have found a full so it goes
    # like this : store all incrementals until we have found a differential
    # or a full, then find the full
    my $query = "
SELECT JobId, FileSet, Level, JobStatus
  FROM Job 
       JOIN FileSet USING (FileSetId)
       JOIN Client USING (ClientId) $filter
 WHERE EndTime <= $date
   AND Client.Name = '$client'
   AND Type IN ('B')
   AND JobStatus IN ('T')
 ORDER BY FileSet, JobTDate DESC";

    my @CurrentJobIds;
    my $result = $self->dbh_selectall_arrayref($query);
    my %progress;
    foreach my $refrow (@$result)
    {
	my $jobid = $refrow->[0];
	my $fileset = $refrow->[1];
	my $level = $refrow->[2];

	defined $progress{$fileset} or $progress{$fileset}='U'; # U for unknown

	next if $progress{$fileset} eq 'F'; # It's over for this fileset...

	if ($level eq 'I')
	{
	    next unless ($progress{$fileset} eq 'U' or $progress{$fileset} eq 'I');
	    push @CurrentJobIds,($jobid);
	}
	elsif ($level eq 'D')
	{
	    next if $progress{$fileset} eq 'D'; # We allready have a differential
	    push @CurrentJobIds,($jobid);
	}
	elsif ($level eq 'F')
	{
	    push @CurrentJobIds,($jobid);
	}

	my $status = $refrow->[3] ;
	if ($status eq 'T') {	           # good end of job
	    $progress{$fileset} = $level;
	}
    }

    return @CurrentJobIds;
}

sub dbh_selectrow_arrayref
{
    my ($self, $query) = @_;
    $self->debug($query, up => 1);
    return $self->{dbh}->selectrow_arrayref($query);
}

# Returns list of versions of a file that could be restored
# returns an array of
# (jobid,fileindex,mtime,size,inchanger,md5,volname,fileid,LinkFI)
# there will be only one jobid in the array of jobids...
sub get_all_file_versions
{
    my ($self,$pathid,$fileid,$client,$see_all,$see_copies)=@_;

    defined $see_all or $see_all=0;
    my $backup_type="";
    if ($see_copies) {
        $backup_type=" copies ";
    }

    my $bc = $self->get_bconsole();
    my $res = $bc->send_one_cmd(".bvfs_versions fnid=$fileid pathid=$pathid " . 
                                "client=\"$client\" jobid=1 $backup_type" .
                                $self->{bvfs_user});

    my @versions;
    # (pathid,fileid,jobid,fid,mtime,size,inchanger,md5,volname,LinkFI );
    # PathId, FilenameId, fileid, jobid, lstat, Md5, VolName, VolInchanger
    foreach my $row (@$res)
    {
        next unless $row =~ /^\d+\t\d+/;
	my ($pathid, $fid, $fileid, $jobid, $lstat, $md5, $volname, $inchanger) 
            = split(/\t/, $row);

	my @attribs = parse_lstat($lstat);
	my $mtime = array_attrib('st_mtime',\@attribs);
	my $size = array_attrib('st_size',\@attribs);
	my $LinkFI = array_attrib('LinkFI',\@attribs);

        #              0     1      2       3        4      5        6
	my @list = ($pathid,$fileid,$jobid, $fid, $mtime, $size, $inchanger,
		    $md5, $volname, $LinkFI);
	push @versions, (\@list);
    }

    # We have the list of all versions of this file.
    # We'll sort it by mtime desc, size, md5, inchanger desc, FileId
    # the rest of the algorithm will be simpler
    # ('FILE:',filename,jobid,fileindex,mtime,size,inchanger,md5,volname)
    @versions = sort { $b->[4] <=> $a->[4]
		    || $a->[5] <=> $b->[5]
		    || $a->[7] cmp $a->[7]
		    || $b->[6] <=> $a->[6]} @versions;

    my @good_versions;
    my %allready_seen_by_mtime;
    my %allready_seen_by_md5;
    # Now we should create a new array with only the interesting records
    foreach my $ref (@versions)
    {
	if ($ref->[7])
	{
	    # The file has a md5. We compare his md5 to other known md5...
	    # We take size into account. It may happen that 2 files
	    # have the same md5sum and are different. size is a supplementary
	    # criterion

            # If we allready have a (better) version
	    next if ( (not $see_all)
	              and $allready_seen_by_md5{$ref->[7] .'-'. $ref->[5]});

	    # we never met this one before...
	    $allready_seen_by_md5{$ref->[7] .'-'. $ref->[5]}=1;
	}
	# Even if it has a md5, we should also work with mtimes
        # We allready have a (better) version
	next if ( (not $see_all)
	          and $allready_seen_by_mtime{$ref->[4] .'-'. $ref->[5]});
	$allready_seen_by_mtime{$ref->[4] .'-'. $ref->[5] . '-' . $ref->[7]}=1;

	# We reached there. The file hasn't been seen.
	push @good_versions,($ref);
    }

    # To be nice with the user, we re-sort good_versions by
    # inchanger desc, mtime desc
    @good_versions = sort { $b->[4] <=> $a->[4]
                         || $b->[2] <=> $a->[2]} @good_versions;

    return \@good_versions;
}
{
    my %attrib_name_id = ( 'st_dev' => 0,'st_ino' => 1,'st_mode' => 2,
			  'st_nlink' => 3,'st_uid' => 4,'st_gid' => 5,
			  'st_rdev' => 6,'st_size' => 7,'st_blksize' => 8,
			  'st_blocks' => 9,'st_atime' => 10,'st_mtime' => 11,
			  'st_ctime' => 12,'LinkFI' => 13,'st_flags' => 14,
			  'data_stream' => 15);;
    sub array_attrib
    {
	my ($attrib,$ref_attrib)=@_;
	return $ref_attrib->[$attrib_name_id{$attrib}];
    }

    sub file_attrib
    {   # $file = [filenameid,listfiles.id,listfiles.Name, File.LStat, File.JobId]

	my ($file, $attrib)=@_;

	if (defined $attrib_name_id{$attrib}) {

	    my @d = split(' ', $file->[3]) ; # TODO : cache this

	    return from_base64($d[$attrib_name_id{$attrib}]);

	} elsif ($attrib eq 'jobid') {

	    return $file->[4];

        } elsif ($attrib eq 'name') {

	    return $file->[2];

	} else	{
	    die "Attribute not known : $attrib.\n";
	}
    }

    sub lstat_attrib
    {
        my ($lstat,$attrib)=@_;
        if ($lstat and defined $attrib_name_id{$attrib})
        {
	    my @d = split(' ', $lstat) ; # TODO : cache this
	    return from_base64($d[$attrib_name_id{$attrib}]);
	}
	return 0;
    }
}

{
    # Base 64 functions, directly from recover.pl.
    # Thanks to
    # Karl Hakimian <hakimian@aha.com>
    # This section is also under GPL v2 or later.
    my @base64_digits;
    my @base64_map;
    my $is_init=0;
    sub init_base64
    {
	@base64_digits = (
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
	'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'
			  );
	@base64_map = (0) x 128;

	for (my $i=0; $i<64; $i++) {
	    $base64_map[ord($base64_digits[$i])] = $i;
	}
	$is_init = 1;
    }

    sub from_base64 {
	if(not $is_init)
	{
	    init_base64();
	}
	my $where = shift;
	my $val = 0;
	my $i = 0;
	my $neg = 0;

	if (substr($where, 0, 1) eq '-') {
	    $neg = 1;
	    $where = substr($where, 1);
	}

	while ($where ne '') {
	    $val *= 64;
	    my $d = substr($where, 0, 1);
	    $val += $base64_map[ord(substr($where, 0, 1))];
	    $where = substr($where, 1);
	}

	return $val;
    }

    sub parse_lstat {
	my ($lstat)=@_;
	my @attribs = split(' ',$lstat);
	foreach my $element (@attribs)
	{
	    $element = from_base64($element);
	}
	return @attribs;
    }
}

# get jobids that the current user can view (ACL)
sub get_jobids
{
  my ($self, @jobid) = @_;
  my $filter = $self->get_client_filter();
  if ($filter) {
    my $jobids = $self->dbh_join(@jobid);
    my $q="
SELECT JobId 
  FROM Job JOIN Client USING (ClientId) $filter 
 WHERE Jobid IN ($jobids)";
    my $res = $self->dbh_selectall_arrayref($q);
    @jobid = map { $_->[0] } @$res;
  }
  return @jobid;
}

################################################################


package main;
use strict;
use POSIX qw/strftime/;
use Bweb;

my $conf = new Bweb::Config(config_file => $Bweb::config_file);
$conf->load();

my $skipdot=0;
if (CGI::param("skipdot")) {
    $skipdot=1;
}

my $bvfs = new Bvfs(info => $conf, skipdot => $skipdot);
my $user = $bvfs->{loginname};
if ($bvfs->{loginname}) {
    $bvfs->{bvfs_user} = " username=\"$bvfs->{loginname}\" ";
} else {
    $bvfs->{bvfs_user} = "";
}
$bvfs->connect_db();

my $action = CGI::param('action') || '';

my $args = $bvfs->get_form('pathid', 'filenameid', 'fileid', 'qdate',
			   'limit', 'offset', 'client');

if ($action eq 'batch') {
    $bvfs->update_cache();
    exit 0;
}

my $pattern = CGI::param('pattern') || '';
if ($pattern =~ /^([\w\d,:\.\-% ]+)$/) {
    $bvfs->set_pattern($1);
}

my $nodir;
if ($conf->{subconf} 
    && scalar(%{$conf->{subconf}}) # we have non empty subconf
    && !$conf->{current_conf})
{
    $nodir=1;
}
# All these functions are returning JSON compatible data
# for javascript parsing

if ($action eq 'list_client') {	# list all client [ ['c1'],['c2']..]
    print CGI::header('application/x-javascript');

    if ($nodir) {
        print "[['Choose a Director first']]\n";
        exit 0;
    }

    my $filter = $bvfs->get_client_filter();
    my $q = "SELECT Name FROM Client $filter";
    my $ret = $bvfs->dbh_selectall_arrayref($q);

    print "[";
    print join(',', map { "['$_->[0]']" } @$ret);
    print "]\n";
    exit 0;
    
} elsif ($action eq 'list_job') {
    # list jobs for a client [[jobid,endtime,'desc'],..]

    print CGI::header('application/x-javascript');
    
    my $filter = $bvfs->get_client_filter();
    my $query = "
 SELECT Job.JobId,Job.EndTime, FileSet.FileSet, Job.Level, Job.JobStatus
  FROM Job JOIN FileSet USING (FileSetId) JOIN Client USING (ClientId) $filter
 WHERE Client.Name = '$args->{client}'
   AND Job.Type = 'B'
   AND JobStatus IN ('f', 'T')
 ORDER BY EndTime desc";
    my $result = $bvfs->dbh_selectall_arrayref($query);

    print "[";

    print join(',', map {
      "[$_->[0], '$_->[1]', '$_->[1] $_->[2] $_->[3] ($_->[4]) $_->[0]']"
      } @$result);

    print "]\n";
    exit 0;

} elsif ($action eq 'list_storage') { 
    print CGI::header('application/x-javascript');

    my $bconsole = $bvfs->get_bconsole();
    my @lst = $bconsole->list_storage();
    print "[";
    print join(',', map { "[ '$_' ]" } @lst);
    print "]\n";
    exit 0;
}

sub fill_table_for_restore
{
    my (@jobid) = @_;

    # in "force" mode, we need the FileId to compute media list
    my $FileId = CGI::param('force')?",FileId":"";

    my $fileid = join(',', grep { /^\d+$/ } CGI::param('fileid'));
    # can get dirid=("10,11", 10, 11)
    my $dirid = join(',', grep { /^\d+$/ } 
                           map { split(/,/) } CGI::param('dirid')) ;
    my $findex = join(',', grep { /^\d+$/ } 
                             map { split(/,|\//) } CGI::param('findex')) ;
    my $jobid = join(',', grep { /^\d+$/ }
                           map { split(/,/) } CGI::param('jobid')) ;
    my $inclause = join(',', @jobid);

    my $b = $bvfs->get_bconsole();
    my $ret = $b->send_one_cmd(".bvfs_restore path=b2$$ fileid=$fileid " .
                               "dirid=$dirid hardlink=$findex jobid=$jobid"
                               . $bvfs->{bvfs_user});
    if (grep (/OK/, @$ret)) {
        return "b2$$";
    }
    return;
}

sub get_media_list_with_dir
{
    my ($table) = @_;
    my $q="
 SELECT DISTINCT VolumeName, Enabled, InChanger
   FROM $table,
    ( -- Get all media from this job
      SELECT MIN(FirstIndex) AS FirstIndex, MAX(LastIndex) AS LastIndex,
             VolumeName, Enabled, Inchanger
        FROM JobMedia JOIN Media USING (MediaId)
       WHERE JobId IN (SELECT DISTINCT JobId FROM $table)
       GROUP BY VolumeName,Enabled,InChanger
    ) AS allmedia
  WHERE $table.FileIndex >= allmedia.FirstIndex
    AND $table.FileIndex <= allmedia.LastIndex
";
    my $lst = $bvfs->dbh_selectall_arrayref($q);
    return $lst;
}

sub get_media_list
{
    my ($jobid, $fileid) = @_;
    my $q="
 SELECT DISTINCT VolumeName, Enabled, InChanger
   FROM File,
    ( -- Get all media from this job
      SELECT MIN(FirstIndex) AS FirstIndex, MAX(LastIndex) AS LastIndex,
             VolumeName, Enabled, Inchanger
        FROM JobMedia JOIN Media USING (MediaId)
       WHERE JobId IN ($jobid)
       GROUP BY VolumeName,Enabled,InChanger
    ) AS allmedia
  WHERE File.FileId IN ($fileid)
    AND File.FileIndex >= allmedia.FirstIndex
    AND File.FileIndex <= allmedia.LastIndex
";
    my $lst = $bvfs->dbh_selectall_arrayref($q);
    return $lst;
}

# get jobid param and apply user filter
my @jobid = $bvfs->get_jobids(grep { /^\d+(,\d+)*$/ } CGI::param('jobid'));

# get jobid from date arg
if (!scalar(@jobid) and $args->{qdate} and $args->{client}) {
    @jobid = $bvfs->set_job_ids_for_date($args->{client}, $args->{qdate});
}

$bvfs->set_curjobids(@jobid);
$bvfs->set_limits($args->{offset}, $args->{limit});

if (!scalar(@jobid)) {
    exit 0;
}

if (CGI::param('init')) { # used when choosing a job
    $bvfs->update_brestore_table(@jobid);
}

my $pathid = CGI::param('node') || CGI::param('pathid') || '';
my $path = CGI::param('path');

if ($pathid =~ /^(\d+)$/) {
    $pathid = $1;
} elsif ($path) {
    $pathid = $bvfs->get_pathid($path);
} else {
    $pathid = $bvfs->get_root();
}
$bvfs->ch_dir($pathid);

#print STDERR "pathid=$pathid\n";

if ($action eq 'restore') {

    # TODO: pouvoir choisir le replace et le jobname
    my $arg = $bvfs->get_form(qw/client storage regexwhere where comment dir/);

    if (!$arg->{client}) {
	print "ERROR: missing client\n";
	exit 1;
    }

    my $table = fill_table_for_restore(@jobid);
    if (!$table) {
	print "ERROR: can create restore table\n";
        exit 1;
    }

    # TODO: remove it after a while
    if ($bvfs->get_db_field('Comment') ne 'Comment') {
        delete $arg->{comment};
    }

    my $bconsole = $bvfs->get_bconsole();
    # TODO: pouvoir choisir le replace et le jobname
    my $jobid = $bconsole->run(client    => $arg->{client},
                               storage   => $arg->{storage},
                               where     => $arg->{where},
                               regexwhere=> $arg->{regexwhere},
                               restore   => 1,
                               comment   => $arg->{comment},
                               file      => "?$table");
    
    $bvfs->dbh_do("DROP TABLE $table");

    if (!$jobid) {
	print CGI::header('text/html');
	$bvfs->display_begin();
	$bvfs->error("Can't start your job:<br/>" . $bconsole->before());
	$bvfs->display_end();
	exit 0;
    }

    sleep(2);

    my $dir='';
    if ($arg->{dir}) {
        $dir=";dir=$arg->{dir}";
    }

    print CGI::redirect("bweb.pl?action=dsp_cur_job;jobid=$jobid$dir") ;
    exit 0;
}
sub escape_quote
{
    my ($str) = @_;
    my %esc = (
        "\n" => '\n',
        "\r" => '\r',
        "\t" => '\t',
        "\f" => '\f',
        "\b" => '\b',
        "\"" => '\"',
        "\\" => '\\\\',
        "\'" => '\\\'',
    );

    if (!$str) {
        return '';
    }

    $str =~ s/([\x22\x5c\n\r\t\f\b])/$esc{$1}/g;
    $str =~ s/\//\\\//g;
    $str =~ s/([\x00-\x08\x0b\x0e-\x1f])/'\\u00' . unpack('H2', $1)/eg;
    return $str;
}

print CGI::header('application/x-javascript');


if ($action eq 'list_files_dirs') {
# fileid, filenameid, pathid, jobid, name, size, mtime, LinkFI
    my $jids = join(",", @jobid);

    my $files = $bvfs->ls_dirs();
    # return ($dirid,$dir_basename,$lstat,$jobid)
    print '[', join(',',
	       map { my @p=Bvfs::parse_lstat($_->[3]); 
		     '[' . join(',', 
				0, # fileid
				0, # filenameid
				$_->[0], # pathid
				"'$jids'", # jobid
				'"' . escape_quote($_->[1]) . '"', # name
				"'" . $p[7] . "'",                 # size
				"'" . strftime('%Y-%m-%d %H:%m:%S', localtime($p[11]||0)) .  "'",
                                0) . # LinkFI
		    ']'; 
	       } @$files);

    print "," if (@$files);
 
    $files = $bvfs->ls_files();
    print join(',',
	       map { my @p=Bvfs::parse_lstat($_->[3]); 
		     '[' . join(',', 
				$_->[1], # fileid
				$_->[0], # fnid
				$pathid, # pathid
				$_->[4], # jobid
                                '"' . escape_quote($_->[2]) . '"', # name
				"'" . $p[7] . "'",
				"'" . strftime('%Y-%m-%d %H:%m:%S', localtime($p[11])) .  "'",
                                $p[13]) . # LinkFI
		    ']'; 
	       } @$files);
    print "]\n";

} elsif ($action eq 'list_files') {
    print "[[0,0,0,0,'.',4096,'1970-01-01 00:00:00'],";
    my $files = $bvfs->ls_files();
#	[ 1, 2, 3, "Bill",  10, '2007-01-01 00:00:00'],
#   File.FilenameId, listfiles.id, listfiles.Name, File.LStat, File.JobId,LinkFI

    print join(',',
	       map { my @p=Bvfs::parse_lstat($_->[3]); 
		     '[' . join(',', 
				$_->[1],
				$_->[0],
				$pathid,
				$_->[4],
                                '"' . escape_quote($_->[2]) . '"', # name
				"'" . $p[7] . "'",
				"'" . strftime('%Y-%m-%d %H:%m:%S', localtime($p[11])) .  "'",
                                $p[13]) . # LinkFI
		    ']'; 
	       } @$files);
    print "]\n";

} elsif ($action eq 'list_dirs') {

    print "[";
    my $dirs = $bvfs->ls_dirs();

    # return ($dirid,$dir_basename,$lstat,$jobid)

    print join(',',
	       map { "{ 'jobid': '$bvfs->{curjobids}', 'id': '$_->[0]'," . 
                         "'text': '" . escape_quote($_->[1]) . "', 'cls':'folder'}" } 
	       @$dirs);
    print "]\n";

} elsif ($action eq 'list_versions') {

    my $vafv = CGI::param('vafv') || 'false'; # view all file versions
    $vafv = ($vafv eq 'false')?0:1;

    my $vcopies = CGI::param('vcopies') || 'false'; # view copies file versions
    $vcopies = ($vcopies eq 'false')?0:1;

    print "[";
    #   0     1      2     3   4     5     6        7      8     9
    #(pathid,fileid,jobid,fid,mtime,size,inchanger,md5,volname,LinkFI );
    my $files = $bvfs->get_all_file_versions($args->{pathid}, $args->{filenameid}, $args->{client}, $vafv, $vcopies);
    print join(',',
	       map { "[ $_->[1], $_->[3], $_->[0], $_->[2], '$_->[8]', $_->[6], '$_->[7]', $_->[5],'" . strftime('%Y-%m-%d %H:%m:%S', localtime($_->[4])) . "',$_->[9]]" }
	       @$files);
    print "]\n";

# this action is used when the restore box appear, we can display
# the media list that will be needed for restore
} elsif ($action eq 'get_media') {
    my ($jobid, $fileid, $table);
    my $lst;

    # in this mode, we compute the result to get all needed media
#    print STDERR "force=", CGI::param('force'), "\n";
    if (CGI::param('force')) {
        $table = fill_table_for_restore(@jobid);
        if (!$table) {
            exit 1;
        }
        # mysql is very slow without this index...
        if ($bvfs->dbh_is_mysql()) {
            $bvfs->dbh_do("CREATE INDEX idx_$table ON $table (JobId)");
        }
        $lst = get_media_list_with_dir($table);
    } else {
        $jobid = join(',', @jobid);
        $fileid = join(',', grep { /^\d+(,\d+)*$/ } CGI::param('fileid'));
        $lst = get_media_list($jobid, $fileid);
    }        
    
    if ($lst) {
        print "[";
        print join(',', map { "['$_->[0]',$_->[1],$_->[2]]" } @$lst);
        print "]\n";
    }

    if ($table) {
        my $b = $bvfs->get_bconsole();
        $b->send_one_cmd(".bvfs_cleanup path=b2$$");
    }

}

__END__

CREATE VIEW files AS
 SELECT path || name AS name,pathid,filenameid,fileid,jobid
   FROM File JOIN FileName USING (FilenameId) JOIN Path USING (PathId);

SELECT 'drop table ' || tablename || ';'
    FROM pg_tables WHERE tablename ~ '^b[0-9]';
