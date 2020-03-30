! Conformal Cubic Atmospheric Model
    
! Copyright 2015-2020 Commonwealth Scientific Industrial Research Organisation (CSIRO)
    
! This file is part of the Conformal Cubic Atmospheric Model (CCAM)
!
! CCAM is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! CCAM is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with CCAM.  If not, see <http://www.gnu.org/licenses/>.

!------------------------------------------------------------------------------
    
Program igbpveg

! This code creates CCAM vegie data using the 1km SiB dataset

Implicit None

include 'version.h'

character(len=1024), dimension(:,:), allocatable :: options
character(len=1024), dimension(15) :: fname
character(len=1024) topofile
character(len=1024) landtypeout
character(len=1024) newtopofile
character(len=1024) outputmode
character(len=1024) veginput, soilinput, laiinput, albvisinput, albnirinput
character(len=1024) pftconfig, mapconfig, atebconfig
character(len=1024) user_veginput, user_laiinput
character(len=1024) soilconfig, change_landuse
integer binlimit, nopts, month, year
integer outmode
logical fastigbp,igbplsmask,ozlaipatch,tile,zerozs,ovegfrac

namelist/vegnml/ topofile,fastigbp,                  &
                 landtypeout,igbplsmask,newtopofile, &
                 binlimit,month,ozlaipatch,          &  
                 tile,outputmode, veginput,          &
                 soilinput, laiinput, albvisinput,   &
                 albnirinput,pftconfig,mapconfig,    &
                 atebconfig,                         &
                 user_veginput, user_laiinput,       &
                 ovegfrac,zerozs,soilconfig,         &
                 change_landuse,year

! Start banner
write(6,*) "=============================================================================="
write(6,*) "CCAM: Starting igbpveg"
write(6,*) "=============================================================================="

write(6,*) 'IGBPVEG - IGBP 1km to CC grid'
write(6,*) version

#ifndef stacklimit
! For linux only - removes stacklimit on all processors
call setstacklimit(-1)
#endif 

! Read switches
nopts=1
allocate (options(nopts,2))
options(:,1) = (/ '-s' /)
options(:,2) = ''

call readswitch(options,nopts)
call defaults(options,nopts)

veginput='gigbp2_0ll.img'
soilinput='usda4.img'
laiinput=''
albvisinput='salbvis223.img'
albnirinput='salbnir223.img'
outputmode=''
pftconfig=''
mapconfig=''
atebconfig=''
ozlaipatch=.false.
user_veginput=''
ovegfrac=.false.
user_laiinput=''
zerozs=.true.
soilconfig=''
change_landuse=''
month=0
year=0

! Read namelist
write(6,*) 'Input &vegnml namelist'
read(5,NML=vegnml)
write(6,*) 'Namelist accepted'

! Generate veg data
fname(1)=topofile
fname(2)=landtypeout
fname(3)=newtopofile
fname(4)=veginput
fname(5)=soilinput
fname(6)=laiinput
fname(7)=albvisinput
fname(8)=albnirinput
fname(9)=pftconfig
fname(10)=mapconfig
fname(11)=user_veginput
fname(12)=user_laiinput
fname(13)=atebconfig
fname(14)=soilconfig
fname(15)=change_landuse

outmode=1
if ( outputmode=='igbp' ) then
  outmode=0
end if

call createveg(options,nopts,fname,fastigbp,igbplsmask,ozlaipatch,tile,month,year, &
               binlimit,outmode,zerozs,ovegfrac)

deallocate(options)

! Complete
write(6,*) "CCAM: igbpveg completed successfully"
call finishbanner

stop
end

subroutine finishbanner

implicit none

! End banner
write(6,*) "=============================================================================="
write(6,*) "CCAM: Finished igbpveg"
write(6,*) "=============================================================================="

return
end

    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine displays the help message
!

Subroutine help()

Implicit None

Write(6,*)
Write(6,*) "Usage:"
Write(6,*) "  igbpveg -s size < igbpveg.nml"
Write(6,*)
Write(6,*) "Options:"
Write(6,*) "  -s size      size of array used for reading SiB data"
Write(6,*) "               (typically =500).  The larger the array, the"
Write(6,*) "               faster and more accurate the output."
Write(6,*)
Write(6,*) "Namelist:"
Write(6,*) "  The namelist specifies what data to store and the filenames"
Write(6,*) "  to use.  For example:"
Write(6,*)
Write(6,*) '  &vegnml'
Write(6,*) '    month=0'
write(6,*) '    year=0'
Write(6,*) '    topofile="topout"'
Write(6,*) '    newtopofile="newtopout"'
Write(6,*) '    landtypeout="veg"'
Write(6,*) '    veginput="gigbp2_0ll.img"'
Write(6,*) '    soilinput="usda4.img"'
Write(6,*) '    laiinput="slai01.img"'
Write(6,*) '    albvisinput="salbvis223.img"'
Write(6,*) '    albnirinput="salbnir223.img"'
Write(6,*) '    fastigbp=t'
Write(6,*) '    igbplsmask=t'
!Write(6,*) '    ozlaipatch=f'
Write(6,*) '    tile=t'
Write(6,*) '    binlimit=2'
write(6,*) '    zerozs=.true.'
Write(6,*) '    outputmode="cablepft"'
write(6,*) '    pftconfig="def_veg_params.txt"'
write(6,*) '    mapconfig="def_veg_mapping.txt"'
write(6,*) '    atebconfig="def_urban_params.txt"'
write(6,*) '    soilconfig="def_soil_params.txt"'
write(6,*) '    user_veginput="myveg.nc"'
write(6,*) '    user_laiinput="mylai.nc"'
write(6,*) '    ovegfrac=.false.'
write(6,*) '    change_landuse="IPCC_ssp370.nc"'
Write(6,*) '  &end'
Write(6,*)
Write(6,*) '  where:'
Write(6,*) '    month          = the month to process (1-12, 0=all)'
write(6,*) '    year           = the year to process for land-use change'
Write(6,*) '    topofile       = topography (input) file'
Write(6,*) '    newtopofile    = Output topography file name'
Write(6,*) '                     (if igbplsmask=t)'
Write(6,*) '    landtypeout    = Land-use filename'
Write(6,*) '    veginput       = Location of IGBP input file'
Write(6,*) '    soilinput      = Location of USDA input file'
Write(6,*) '    laiinput       = Location of LAI input file for month>0'
Write(6,*) '                     or path to LAI files for month=0'
Write(6,*) '    albvisinput    = Location of VIS Albedo input file'
Write(6,*) '    albnirinput    = Location of NIR Albedo input file'
Write(6,*) '    fastigbp       = Turn on fastigbp mode (see notes below)'
Write(6,*) '    igbplsmask     = Define land/sea mask from IGBP dataset'
!Write(6,*) '    ozlaipath     = Use CSIRO LAI dataset for Australia'
Write(6,*) '    tile           = Seperate land cover into tiles'
Write(6,*) '    binlimit       = The minimum ratio between the grid'
Write(6,*) '                     length scale and the length scale of'
Write(6,*) '                     the aggregated land-use data (see notes'
Write(6,*) '                     below).'
write(6,*) '    zerozs         = Set orography height to zero for oceans'
write(6,*) '                     (default = true)'
Write(6,*) '    outputmode     = format of output file.'
Write(6,*) '                     igbp     Use IGBP classes'
Write(6,*) '                     cablepft Use CABLE PFTs (default)'
write(6,*) '    pftconfig      = Location of the PFT definition file'
write(6,*) '                     Use standard CABLE PFT file with the'
write(6,*) '                     first index to define the reference'
write(6,*) '                     CSIRO PFT (1-17)'
write(6,*) '    mapconfig      = Location of the mapping file to'
write(6,*) '                     convert indices from veginput to'
write(6,*) '                     PFTs defined in pftconfig'
write(6,*) '    atebconfig     = Location of the aTEB definition file'
write(6,*) '    user_veginput  = Location of user modified vegetation'
write(6,*) '    user_laiinput  = Location of user modified LAI'
write(6,*) '    ovegfrac       = Use veg fraction for each type from user file'
write(6,*) '    soilconfig     = Location of the Soil definition file'
write(6,*) '    change_landuse = file for time varying land-use'
write(6,*) '                     (default = blank)'
Write(6,*)
Write(6,*) 'NOTES: fastigbp mode will speed up the code by aggregating'
Write(6,*) '       land-use data at a coarser resolution before'
Write(6,*) '       processing.  The degree of aggregation is determined'
Write(6,*) '       by the avaliable memory (i.e., -s switch).   Usually,'
Write(6,*) '       fastigbp is used to test the output and then the'
Write(6,*) '       dataset is subsequently regenerated with fastigbp=f.'
Write(6,*)
Write(6,*) '       During the binning of land-use data, the length scale'
Write(6,*) '       eventually becomes sufficently small so that binlimit'
Write(6,*) '       can no longer be satisfied.  Under these circumstances'
Write(6,*) '       the code will use the minimum length scale of the'
Write(6,*) '       IGBP dataset (e.g., 1km) for all data that is'
Write(6,*) '       subsequently binned.  In the case where the grid scale'
Write(6,*) '       is less than the minimum length scale of the IGBP'
Write(6,*) '       dataset, the code will use the nearest grid point'
Write(6,*) '       instead of binning.'
write(6,*)
write(6,*) '       User modified vegetation and LAI files need to be'
write(6,*) '       formatted so that the order of dimensions is'
write(6,*) '       longitude, latitude, time (shown in reverse when'
write(6,*) '       using ncdump).  12 time-steps are required in the'
write(6,*) '       LAI file when month=0.'
Write(6,*)
call finishbanner
Stop

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determins the default values for the switches
!

Subroutine defaults(options,nopts)

Implicit None

Integer nopts
Character(len=*), dimension(nopts,2), intent(inout) :: options
Integer siz
Integer locate

siz=locate('-s',options(:,1),nopts)

If (options(siz,2)=='') then
  options(siz,2)='500'
End if

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine processes the sib data
!

Subroutine createveg(options,nopts,fname,fastigbp,igbplsmask,ozlaipatch,tile,month,year, &
                     binlimit,outmode,zerozs,ovegfrac)

Use ccinterp

Implicit None

Logical, intent(in) :: fastigbp,igbplsmask,ozlaipatch,tile,zerozs,ovegfrac
Integer, intent(in) :: nopts,binlimit,month,year,outmode
Character(len=*), dimension(nopts,2), intent(in) :: options
Character(len=*), dimension(15), intent(in) :: fname
character(len=1024) filename
Character(len=80), dimension(1:3) :: outputdesc
Character(len=1024) returnoption,csize
Character(len=47) header
Character(len=9) formout
Character(len=2) monthout
real, dimension(:,:,:), allocatable :: vlai
Real, dimension(:,:,:), allocatable :: landdata,soildata,rlld,vfrac,tmp
real, dimension(:,:,:), allocatable :: changedata
Real, dimension(:,:), allocatable :: gridout,lsdata,urbandata,oceandata,albvisdata,albnirdata
real, dimension(:,:), allocatable :: testdata
real, dimension(:,:), allocatable :: rdata
Real, dimension(3,2) :: alonlat
Real, dimension(2) :: lonlat
Real, dimension(1) :: atime
Real, dimension(1) :: alvl
Real schmidt,dsx,ds,urbanfrac
real urbanmaxfrac, urbantotalfrac
real nsum, newsum, change_crop, change_pasture, icefrac
integer, dimension(:,:), allocatable :: idata, urbantype
integer, dimension(:,:,:), allocatable :: vtype
Integer, dimension(2) :: sibdim
Integer, dimension(4) :: dimnum,dimid,dimcount
Integer, dimension(0:4) :: ncidarr
Integer, dimension(6) :: adate
Integer, dimension(2:33) :: varid
integer, dimension(1) :: sibmax
Integer sibsize,tunit,i,j,k,ierr,mthrng
integer tt, n
logical, dimension(:), allocatable :: sermsk

integer :: pft_len = 18
integer :: class_num = 17
integer, parameter :: ch_len = 50 ! also defined in ncwrite.f90
integer pft_dimid, ioerror, jveg
integer maxindex, iposbeg, iposend
integer :: ateb_len = 8
integer ateb_dimid, jateb
integer :: soil_len = 9
integer soil_dimid
integer, dimension(:), allocatable :: mapjveg
integer, dimension(:,:), allocatable :: mapindex
real notused
real, dimension(:), allocatable :: csiropft
real, dimension(:), allocatable :: hc, xfang, leaf_w, leaf_l, canst1
real, dimension(:), allocatable :: shelrb, extkn, vcmax, rpcoef
real, dimension(:), allocatable :: rootbeta, c4frac, vbeta
real, dimension(:), allocatable :: bldheight, hwratio, sigvegc, sigmabld
real, dimension(:), allocatable :: industryfg, trafficfg, roofalpha
real, dimension(:), allocatable :: wallalpha, roadalpha, vegalphac, zovegc
real, dimension(:), allocatable :: infiltration, internalgain, bldtemp
real, dimension(:), allocatable :: heatprop, coolprop
real, dimension(:,:), allocatable :: roofthick, roofcp, roofcond
real, dimension(:,:), allocatable :: wallthick, wallcp, wallcond
real, dimension(:,:), allocatable :: slabthick, slabcp, slabcond
real, dimension(:,:), allocatable :: roadthick, roadcp, roadcond
real, dimension(:), allocatable :: a1gs, d0gs, alpha, convex, cfrd
real, dimension(:), allocatable :: gswmin, conkc0, conko0, ekc, eko, g0, g1
real, dimension(:), allocatable :: zr, clitt
real, dimension(:,:), allocatable :: refl, taul
real, dimension(:,:), allocatable :: mapfrac
real, dimension(:), allocatable :: silt
real, dimension(:), allocatable :: clay
real, dimension(:), allocatable :: sand
real, dimension(:), allocatable :: swilt
real, dimension(:), allocatable :: sfc
real, dimension(:), allocatable :: ssat
real, dimension(:), allocatable :: bch
real, dimension(:), allocatable :: hyds
real, dimension(:), allocatable :: sucs
real, dimension(:), allocatable :: rhosoil
real, dimension(:), allocatable :: css
character(len=ch_len), dimension(:), allocatable :: pft_desc
character(len=ch_len), dimension(:), allocatable :: ateb_desc
character(len=ch_len), dimension(:), allocatable :: soil_desc
character(len=256) :: comments, largestring
character(len=10) :: vegtypetmp
character(len=25) :: vegnametmp, atebtypetmp
character(len=25) :: jdesc, kdesc
character(len=25) :: vname
logical, dimension(:), allocatable :: mapwater, mapice
logical :: testurban, testwater, testice, matchfound
character(len=2) :: dum

mthrng=1
if ( month==0 ) then
  mthrng=12
end if
if ( month<0 .or. month>12 ) then
  write(6,*) "ERROR: Invalid month ",month
  write(6,*) "Must be between 0 and 12"
  call finishbanner
  stop -1
end if

csize=returnoption('-s',options,nopts)
read(csize,FMT=*,IOSTAT=ierr) sibsize
if (ierr/=0) then
  write(6,*) 'ERROR: Invalid array si=ze.  Must be an integer.'
  call finishbanner
  stop -1
end if

! Read topography file
tunit=1
call readtopography(tunit,fname(1),sibdim,lonlat,schmidt,dsx,header)

write(6,*) "Dimension : ",sibdim
write(6,*) "lon0,lat0 : ",lonlat
write(6,*) "Schmidt   : ",schmidt

allocate(gridout(sibdim(1),sibdim(2)),rlld(sibdim(1),sibdim(2),2))

! Determine lat/lon to CC mapping
call ccgetgrid(rlld,gridout,sibdim,lonlat,schmidt,ds)

! read custom PFT file
if ( fname(9)/='' .and. outmode==1 ) then
    
  write(6,*) "Defining user specified CABLE PFTs"
  open(unit=40,file=fname(9),status='old',action='read',iostat=ioerror)
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot open pftconfig file ",trim(fname(9))
    call finishbanner
    stop -1
  end if
    
  read(40,*) comments
  read(40,*) pft_len
  allocate( pft_desc(pft_len) )
  allocate( csiropft(pft_len), xfang(pft_len), leaf_w(pft_len), leaf_l(pft_len) )
  allocate( hc(pft_len), canst1(pft_len), shelrb(pft_len), extkn(pft_len) )
  allocate( vcmax(pft_len), rpcoef(pft_len), rootbeta(pft_len), c4frac(pft_len) )
  allocate( vbeta(pft_len) )
  allocate( refl(pft_len,2), taul(pft_len,2) )
  allocate( a1gs(pft_len), d0gs(pft_len), alpha(pft_len), convex(pft_len), cfrd(pft_len) )
  allocate( gswmin(pft_len), conkc0(pft_len), conko0(pft_len), ekc(pft_len), eko(pft_len), g0(pft_len), g1(pft_len) )
  allocate( zr(pft_len), clitt(pft_len) )

  do i = 1,pft_len
        
    read(40,*,iostat=ioerror) jveg, vegtypetmp, vegnametmp
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 1 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    write(6,*) "Processing PFT ",trim(vegnametmp)
    if ( jveg<1 .or. jveg>17 ) then
      write(6,*) "ERROR: Error processing ",trim(vegnametmp)
      write(6,*) "in pftconfig ",trim(fname(9))
      write(6,*) "veg index should match a CSIRO PFT from 1-17"
      write(6,*) "whereas veg was read as ",jveg
      call finishbanner
      stop -1
    end if
    csiropft(i) = real(jveg)
    pft_desc(i) = vegnametmp
        
    read(40,*,iostat=ioerror) hc(i), xfang(i), leaf_w(i), leaf_l(i), c4frac(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 2 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) refl(i,1), refl(i,2)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 3 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) taul(i,1), taul(i,2)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 4 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) notused, notused, notused, notused
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 5 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) notused, notused, canst1(i), shelrb(i), notused, extkn(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 6 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) vcmax(i), notused, rpcoef(i), notused
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 7 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) notused, notused, vbeta(i), rootbeta(i), zr(i), clitt(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 8 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) notused, notused, notused, notused, notused
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 9 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) notused, notused, notused, notused, notused
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 10 of PFT number ",i,"/",pft_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) a1gs(i), d0gs(i), alpha(i), convex(i), cfrd(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 11 of PFT number ",i,"/",pft_len
      write(6,*) "Possibly using old PFT file format"
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) gswmin(i), conkc0(i), conko0(i), ekc(i), eko(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 12 of PFT number ",i,"/",pft_len
      write(6,*) "Possibly using old PFT file format"
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) g0(i), g1(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read pftconfig file ",trim(fname(9))
      write(6,*) "Formatting error in line 13 of PFT number ",i,"/",pft_len
      write(6,*) "Possibly using old PFT file format"
      call finishbanner
      stop -1
    end if
      
  end do

  close(40)
    
else
      
  write(6,*) "Defining default CABLE PFTs"
  pft_len=18  
  allocate( pft_desc(pft_len) )
  allocate( csiropft(pft_len), xfang(pft_len), leaf_w(pft_len), leaf_l(pft_len) )
  allocate( hc(pft_len), canst1(pft_len), shelrb(pft_len), extkn(pft_len) )
  allocate( vcmax(pft_len), rpcoef(pft_len), rootbeta(pft_len), c4frac(pft_len) )
  allocate( vbeta(pft_len) )
  allocate( refl(pft_len,2), taul(pft_len,2) )
  allocate( a1gs(pft_len), d0gs(pft_len), alpha(pft_len), convex(pft_len), cfrd(pft_len) )
  allocate( gswmin(pft_len), conkc0(pft_len), conko0(pft_len), ekc(pft_len), eko(pft_len), g0(pft_len), g1(pft_len) )
  allocate( zr(pft_len), clitt(pft_len) )
  pft_desc(1) = "evergreen_needleleaf"  
  pft_desc(2) = "evergreen_broadleaf"
  pft_desc(3) = "deciduous_needleleaf"
  pft_desc(4) = "deciduous_broadleaf"
  pft_desc(5) = "shrub"
  pft_desc(6) = "C3_grassland"
  pft_desc(7) = "C4_grassland"
  pft_desc(8) = "tundra"
  pft_desc(9) = "C3_cropland"
  pft_desc(10) = "C4_cropland"
  pft_desc(11) = "wetland"
  pft_desc(12) = "empty"
  pft_desc(13) = "empty"
  pft_desc(14) = "barren"
  pft_desc(15) = "(Urban-generic)"
  pft_desc(16) = "lakes"
  pft_desc(17) = "ice"
  pft_desc(18) = "evergreen_broadleaf_sava"
  csiropft=(/ 1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12., 13., 14., 15., 16., 17., 2. /)
  hc    =(/   17.,  35.,  15.5,  20.,   0.6, 0.567, 0.567, 0.567, 0.55, 0.55, 0.567,  0.2, 6.017,  0.2,  0.2,  0.2,  0.2, 17. /)
  xfang =(/  0.01,  0.1,  0.01, 0.25,  0.01,  -0.3,  -0.3,  -0.3, -0.3, -0.3,  -0.3,  0.1,    0.,   0.,   0.,   0.,   0., 0.1 /)
  leaf_w=(/ 0.001, 0.05, 0.001, 0.08, 0.005,  0.01,  0.01,  0.01, 0.01, 0.01,  0.01, 0.03, 0.015, 0.00,   0.,   0.,   0., 0.05 /)
  leaf_l=(/ 0.055, 0.10, 0.040, 0.15, 0.100,  0.30,  0.30,  0.30, 0.30, 0.30,  0.30, 0.30, 0.242, 0.03, 0.03, 0.03, 0.03, 0.10 /)
  canst1=0.1
  shelrb=2.
  extkn=0.001
  refl(:,1)=(/ 0.062,0.076,0.056,0.092,0.100,0.110,0.100,0.117,0.100,0.090,0.108,0.055,0.091,0.238,0.143,0.143,0.159,0.076 /)
  refl(:,2)=(/ 0.302,0.350,0.275,0.380,0.400,0.470,0.400,0.343,0.400,0.360,0.343,0.190,0.310,0.457,0.275,0.275,0.305,0.350 /)
  taul(:,1)=(/ 0.050,0.050,0.045,0.050,0.050,0.070,0.100,0.080,0.100,0.090,0.075,0.023,0.059,0.039,0.023,0.023,0.026,0.050 /)
  taul(:,2)=(/ 0.100,0.250,0.144,0.250,0.240,0.250,0.150,0.124,0.150,0.225,0.146,0.198,0.163,0.189,0.113,0.113,0.113,0.250 /)
  vcmax=(/ 40.E-6,55.E-6,40.E-6,60.E-6,40.E-6,60.E-6,10.E-6,40.E-6,80.E-6,80.E-6,60.E-6,17.E-6,1.E-6,17.E-6,17.E-6,17.E-6, &
           17.E-6,55.E-6 /)
  rpcoef=0.0832
  rootbeta=(/ 0.943,0.962,0.966,0.961,0.964,0.943,0.943,0.943,0.961,0.961,0.943,0.975,0.961,0.961,0.961,0.961,0.961,0.962 /)
  c4frac=(/ 0., 0., 0., 0., 0., 0., 1., 0., 0., 1., 0., 0., 0., 0., 0., 0., 0., 0. /)
  vbeta=(/ 2., 2., 2., 2., 4., 4., 4., 4., 2., 2., 4., 4., 2., 4., 4., 4., 4., 2. /)
  a1gs=(/ 9., 9., 9., 9., 9., 9., 4., 9., 9., 4., 9., 9., 9., 9., 9., 9., 9., 9. /)
  d0gs=1500.
  alpha=(/ 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.05, 0.2, 0.2, 0.05, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2 /)
  convex=(/ 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.8, 0.01, 0.01, 0.8, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01 /)
  cfrd=(/ 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.025, 0.015, 0.015, 0.025, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, &
          0.015, 0.015 /)
  gswmin=(/ 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.04, 0.01, 0.01, 0.04, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01 /)
  conkc0=302.e-6
  conko0=256.e-3
  ekc=59430.
  eko=36000.
  g0=0.
  g1=(/ 2.346064, 4.114762, 2.346064, 4.447321, 4.694803, 5.248500, 1.616178, 2.222156, 5.789377, 1.616178, 5.248500, 5.248500, &
        0.000000, 5.248500, 5.248500, 5.248500, 5.248500, 2.346064 /)
  zr=(/ 1.8, 3., 2., 2., 2.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.8, 3.1, 3., 1., 1., 1., 1., 3. /)
  clitt=(/ 20., 6., 10., 13., 2., 2., 0.3, 0.3, 0., 0., 2., 2., 0., 0., 0., 0., 0., 6. /) 
  
end if

! process soil parameters
if ( fname(14)/='' .and. outmode==1 ) then
    
  write(6,*) "Defining user specified soil parameters"
  open(unit=40,file=fname(14),status='old',action='read',iostat=ioerror)
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot open soilconfig file ",trim(fname(14))
    call finishbanner
    stop -1
  end if
  
  read(40,*) comments
  read(40,*) comments
  read(40,*) soil_len

  allocate( soil_desc(soil_len) )
  allocate( silt(soil_len) )
  allocate( clay(soil_len) )
  allocate( sand(soil_len) )
  allocate( swilt(soil_len) )
  allocate( sfc(soil_len) )
  allocate( ssat(soil_len) )
  allocate( bch(soil_len) )
  allocate( hyds(soil_len) )
  allocate( sucs(soil_len) )
  allocate( rhosoil(soil_len) )
  allocate( css(soil_len) )

  read(40,*) comments
  read(40,*) comments
  do i = 1,soil_len
    read(40,'(a2,i6,a)',iostat=ioerror) dum, j, soil_desc(i) 
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
      write(6,*) "Formatting error in line",5+i,"reading soil type description"
      if ( i /= j ) then
        write(6,*) "soil index is not sequential"
      end if
      call finishbanner
      stop -1
    end if
  end do
  read(40,*) comments
  read(40,*) comments
  read(40,*,iostat=ioerror) silt
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",7+soil_len+1,"reading silt fraction"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) clay
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",8+soil_len+1,"reading clay fraction"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) sand
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",9+soil_len+1,"reading sand fraction"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) swilt
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",10+soil_len+1,"reading swilt"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) sfc
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",11+soil_len+1,"reading sfc"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) ssat
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",12+soil_len+1,"reading ssat"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) bch
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",13+soil_len+1,"reading bch"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) hyds
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",14+soil_len+1,"reading hyds"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) sucs
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",15+soil_len+1,"reading sucs"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) rhosoil
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",16+soil_len+1,"reading rhosoil"
    call finishbanner
    stop -1
  end if
  read(40,*,iostat=ioerror) css
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot read soilconfig file ",trim(fname(14))
    write(6,*) "Formatting error in line",17+soil_len+1,"reading css"
    call finishbanner
    stop -1
  end if

else
  
  write(6,*) "Defining default soil parameters"
  soil_len = 9
  allocate( soil_desc(soil_len) )
  allocate( silt(soil_len) )
  allocate( clay(soil_len) )
  allocate( sand(soil_len) )
  allocate( swilt(soil_len) )
  allocate( sfc(soil_len) )
  allocate( ssat(soil_len) )
  allocate( bch(soil_len) )
  allocate( hyds(soil_len) )
  allocate( sucs(soil_len) )
  allocate( rhosoil(soil_len) )
  allocate( css(soil_len) )

  soil_desc(1) = "Coarse sand/Loamy sand"
  soil_desc(2) = "Medium clay loam/silty clay loam/silt loam"
  soil_desc(3) = "Fine clay"
  soil_desc(4) = "Coarse-medium sandy loam/loam"
  soil_desc(5) = "Coarse-fine sandy clay"
  soil_desc(6) = "Medium-fine silty clay"
  soil_desc(7) = "Coarse-medium-fine sandy clay loam"
  soil_desc(8) = "Organic peat"
  soil_desc(9) = "Permanent ice"
  swilt = (/ .072, .216, .286, .135, .219, .283, .175, .395, .216 /)
  ssat = (/ .398, .479, .482, .443, .426, .482, .420, .451, .479 /)
  sfc = (/ .143, .301, .367, .218, .31 , .37 , .255, .45, .301 /)
  bch = (/ 4.2, 7.1, 11.4, 5.15, 10.4, 10.4, 7.12, 5.83, 7.1 /)    
  css = (/ 850., 850., 850., 850., 850., 850., 850., 1920., 2100. /) 
  hyds = (/ 166.e-6, 4.e-6, 1.e-6, 21.e-6, 2.e-6, 1.e-6, 6.e-6,800.e-6, 1.e-6 /)
  !rhosoil = (/ 2600., 2600., 2600., 2600., 2600., 2600., 2600., 1300.,  910. /)
  rhosoil = (/ 1600., 1600., 1381., 1373., 1476., 1521., 1373., 1537.,  910. /)     
  sucs = (/ -.106, -.591, -.405, -.348, -.153, -.49, -.299,-.356, -.153 /)
  clay = (/ .09, .3, .67, .2, .42, .48, .27, .17, .30 /)
  sand = (/ .83, .37, .16, .6, .52, .27, .58, .13, .37 /)
  silt = (/ .08, .33, .17, .2, .06, .25, .15, .70, .33 /)

end if

! process urban parameters
if ( fname(13)/='' .and. outmode==1 ) then
    
  write(6,*) "Defining user specified aTEB classes"
  open(unit=40,file=fname(13),status='old',action='read',iostat=ioerror)
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot open atebconfig file ",trim(fname(13))
    call finishbanner
    stop -1
  end if
  
  read(40,*) comments
  read(40,*) ateb_len
  
  if ( ateb_len>99 ) then
    write(6,*) "ERROR: Maximum number of aTEB classes is 99"
    call finishbanner
    stop -1
  end if
  !if ( ateb_len/=8 ) then
  !  write(6,*) "ERROR: aTEB requires 8 classes"
  !  call finishbanner
  !  stop -1
  !end if
  
  allocate( ateb_desc(ateb_len) )
  allocate( bldheight(ateb_len), hwratio(ateb_len), sigvegc(ateb_len), sigmabld(ateb_len) )
  allocate( industryfg(ateb_len), trafficfg(ateb_len), roofalpha(ateb_len) )
  allocate( wallalpha(ateb_len), roadalpha(ateb_len), vegalphac(ateb_len), zovegc(ateb_len) )
  allocate( infiltration(ateb_len), internalgain(ateb_len), bldtemp(ateb_len) )
  allocate( heatprop(ateb_len), coolprop(ateb_len) )
  allocate( roofthick(ateb_len,4), roofcp(ateb_len,4), roofcond(ateb_len,4) )
  allocate( wallthick(ateb_len,4), wallcp(ateb_len,4), wallcond(ateb_len,4) )
  allocate( slabthick(ateb_len,4), slabcp(ateb_len,4), slabcond(ateb_len,4) )
  allocate( roadthick(ateb_len,4), roadcp(ateb_len,4), roadcond(ateb_len,4) )
  roofthick(:,1) = 0.01
  roofthick(:,2) = 0.09
  roofthick(:,3) = 0.40
  roofthick(:,4) = 0.10
  roofcp(:,1) = 2.11E6
  roofcp(:,2) = 2.11E6
  roofcp(:,3) = 0.28E6
  roofcp(:,4) = 0.29E6
  roofcond(:,1) = 1.5100
  roofcond(:,2) = 1.5100
  roofcond(:,3) = 0.0800
  roofcond(:,4) = 0.0500
  wallthick(:,1) = 0.01
  wallthick(:,2) = 0.04
  wallthick(:,3) = 0.10
  wallthick(:,4) = 0.05
  wallcp(:,1) = 1.55E6
  wallcp(:,2) = 1.55E6
  wallcp(:,3) = 1.55E6
  wallcp(:,4) = 0.29E6
  wallcond(:,1) = 0.9338
  wallcond(:,2) = 0.9338
  wallcond(:,3) = 0.9338
  wallcond(:,4) = 0.0500
  slabthick(:,1) = 0.05
  slabthick(:,2) = 0.05
  slabthick(:,3) = 0.05
  slabthick(:,4) = 0.05
  slabcp(:,1) = 1.55E6
  slabcp(:,2) = 1.55E6
  slabcp(:,3) = 1.55E6
  slabcp(:,4) = 1.55E6
  slabcond(:,1) = 0.9338
  slabcond(:,2) = 0.9338
  slabcond(:,3) = 0.9338
  slabcond(:,4) = 0.9338
  roadthick(:,1) = 0.01
  roadthick(:,2) = 0.04
  roadthick(:,3) = 0.45
  roadthick(:,4) = 3.5
  roadcp(:,1) = 1.94E6
  roadcp(:,2) = 1.94E6
  roadcp(:,3) = 1.28E6
  roadcp(:,4) = 1.28E6
  roadcond(:,1) = 0.7454
  roadcond(:,2) = 0.7454
  roadcond(:,3) = 0.2513
  roadcond(:,4) = 0.2513
  infiltration(:) = 0.5
  internalgain(:) = 5.
  bldtemp(:) = 291.16
  heatprop(:) = 0.5
  coolprop(:) = 0.5
  if ( ateb_len==8 ) then
    ! backwards compatibility  
    heatprop(1:8) = (/ 0.5, 0.5, 0.5, 0.5, 1., 0., 0., 0. /)
    coolprop(1:8) = (/ 0.5, 0.5, 0.5, 0.5, 1., 0., 0., 0. /)
  end if  
  
  do i = 1,ateb_len
        
    read(40,*,iostat=ioerror) jateb, ateb_desc(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
      write(6,*) "Formatting error in line 1 of urban class ",i,"/",ateb_len
      call finishbanner
      stop -1
    end if
    write(6,*) "Processing aTEB class ",trim(ateb_desc(i))
        
    read(40,*,iostat=ioerror) bldheight(i), hwratio(i), sigvegc(i), sigmabld(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
      write(6,*) "Formatting error in line 2 of urban class ",i,"/",ateb_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) industryfg(i), trafficfg(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
      write(6,*) "Formatting error in line 3 of urban class ",i,"/",ateb_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) roofalpha(i), wallalpha(i), roadalpha(i), vegalphac(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
      write(6,*) "Formatting error in line 4 of urban class ",i,"/",ateb_len
      call finishbanner
      stop -1
    end if
    read(40,*,iostat=ioerror) zovegc(i)
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
      write(6,*) "Formatting error in line 5 of urban class ",i,"/",ateb_len
      call finishbanner
      stop -1
    end if
    
    ! v2 format    
    read(40,*,iostat=ioerror) roofthick(i,1),roofthick(i,2),roofthick(i,3),roofthick(i,4)
    if ( ioerror==0 ) then
      read(40,*,iostat=ioerror) roofcp(i,1),roofcp(i,2),roofcp(i,3),roofcp(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 7 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if  
      read(40,*,iostat=ioerror) roofcond(i,1),roofcond(i,2),roofcond(i,3),roofcond(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 8 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if 
      read(40,*,iostat=ioerror) wallthick(i,1),wallthick(i,2),wallthick(i,3),wallthick(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 9 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if 
      read(40,*,iostat=ioerror) wallcp(i,1),wallcp(i,2),wallcp(i,3),wallcp(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 10 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if  
      read(40,*,iostat=ioerror) wallcond(i,1),wallcond(i,2),wallcond(i,3),wallcond(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 11 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if 
      read(40,*,iostat=ioerror) slabthick(i,1),slabthick(i,2),slabthick(i,3),slabthick(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 12 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if 
      read(40,*,iostat=ioerror) slabcp(i,1),slabcp(i,2),slabcp(i,3),slabcp(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 13 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if  
      read(40,*,iostat=ioerror) slabcond(i,1),slabcond(i,2),slabcond(i,3),slabcond(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 14 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if 
      read(40,*,iostat=ioerror) roadthick(i,1),roadthick(i,2),roadthick(i,3),roadthick(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 15 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if 
      read(40,*,iostat=ioerror) roadcp(i,1),roadcp(i,2),roadcp(i,3),roadcp(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 16 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if  
      read(40,*,iostat=ioerror) roadcond(i,1),roadcond(i,2),roadcond(i,3),roadcond(i,4)    
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 17 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if 
    end if
    
    ! v3 format
    read(40,*,iostat=ioerror) infiltration(i),internalgain(i),bldtemp(i)
    if ( ioerror==0 ) then
      read(40,*,iostat=ioerror) heatprop(i),coolprop(i)
      if ( ioerror/=0 ) then
        write(6,*) "ERROR: Cannot read atebconfig file ",trim(fname(13))
        write(6,*) "Formatting error in line 19 of urban class ",i,"/",ateb_len
        call finishbanner
        stop -1
      end if 
    end if
      
  end do
  
  close(40) 
  
else
  write(6,*) "Defining default aTEB classes"  
  ateb_len = 8
  allocate( ateb_desc(ateb_len) )
  allocate( bldheight(ateb_len), hwratio(ateb_len), sigvegc(ateb_len), sigmabld(ateb_len) )
  allocate( industryfg(ateb_len), trafficfg(ateb_len), roofalpha(ateb_len) )
  allocate( wallalpha(ateb_len), roadalpha(ateb_len), vegalphac(ateb_len), zovegc(ateb_len) )
  allocate( infiltration(ateb_len), internalgain(ateb_len), bldtemp(ateb_len) )
  allocate( heatprop(ateb_len), coolprop(ateb_len) )
  allocate( roofthick(ateb_len,4), roofcp(ateb_len,4), roofcond(ateb_len,4) )
  allocate( wallthick(ateb_len,4), wallcp(ateb_len,4), wallcond(ateb_len,4) )
  allocate( slabthick(ateb_len,4), slabcp(ateb_len,4), slabcond(ateb_len,4) )
  allocate( roadthick(ateb_len,4), roadcp(ateb_len,4), roadcond(ateb_len,4) )
  ateb_desc(1) = "Urban-generic"
  ateb_desc(2) = "Urban-low"
  ateb_desc(3) = "Urban-medium"
  ateb_desc(4) = "Urban-high"
  ateb_desc(5) = "Urban-cbd"
  ateb_desc(6) = "Industrial-low"
  ateb_desc(7) = "Industrial-medium"
  ateb_desc(8) = "Industrial-high"
  bldheight(:) = (/ 6.,   4.,   6.,   8.,  18.,   4.,   8.,  12. /)
  hwratio(:) = (/ 0.4,  0.2,  0.4,  0.6,   2.,  0.5,   1.,  1.5 /)
  sigvegc(:) = (/ 0.38, 0.45, 0.38, 0.34, 0.05, 0.40, 0.30, 0.20 /)
  sigmabld(:) = (/ 0.45, 0.40, 0.45, 0.46, 0.65, 0.40, 0.45, 0.50 /)
  industryfg(:) = (/ 0.,   0.,   0.,   0.,   0.,  10.,  20.,  30. /)
  trafficfg(:) = (/ 1.5,  1.5,  1.5,  1.5,  1.5,  1.5,  1.5,  1.5 /)
  roofalpha(:) = (/ 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20 /)
  wallalpha(:) = (/ 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30 /)
  roadalpha(:) = (/ 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10 /)
  vegalphac(:) = (/ 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.20 /)
  zovegc(:) = (/ 0.1,   0.1,   0.1,   0.1,   0.1,   0.1,   0.1,   0.1 /)
  roofthick(:,1) = 0.01
  roofthick(:,2) = 0.09
  roofthick(:,3) = 0.40
  roofthick(:,4) = 0.10
  roofcp(:,1) = 2.11E6
  roofcp(:,2) = 2.11E6
  roofcp(:,3) = 0.28E6
  roofcp(:,4) = 0.29E6
  roofcond(:,1) = 1.5100
  roofcond(:,2) = 1.5100
  roofcond(:,3) = 0.0800
  roofcond(:,4) = 0.0500
  wallthick(:,1) = 0.01
  wallthick(:,2) = 0.04
  wallthick(:,3) = 0.10
  wallthick(:,4) = 0.05
  wallcp(:,1) = 1.55E6
  wallcp(:,2) = 1.55E6
  wallcp(:,3) = 1.55E6
  wallcp(:,4) = 0.29E6
  wallcond(:,1) = 0.9338
  wallcond(:,2) = 0.9338
  wallcond(:,3) = 0.9338
  wallcond(:,4) = 0.0500
  slabthick(:,1) = 0.05
  slabthick(:,2) = 0.05
  slabthick(:,3) = 0.05
  slabthick(:,4) = 0.05
  slabcp(:,1) = 1.55E6
  slabcp(:,2) = 1.55E6
  slabcp(:,3) = 1.55E6
  slabcp(:,4) = 1.55E6
  slabcond(:,1) = 0.9338
  slabcond(:,2) = 0.9338
  slabcond(:,3) = 0.9338
  slabcond(:,4) = 0.9338
  roadthick(:,1) = 0.01
  roadthick(:,2) = 0.04
  roadthick(:,3) = 0.45
  roadthick(:,4) = 3.5
  roadcp(:,1) = 1.94E6
  roadcp(:,2) = 1.94E6
  roadcp(:,3) = 1.28E6
  roadcp(:,4) = 1.28E6
  roadcond(:,1) = 0.7454
  roadcond(:,2) = 0.7454
  roadcond(:,3) = 0.2513
  roadcond(:,4) = 0.2513
  infiltration(:) = 0.5
  internalgain(:) = 5.
  bldtemp(:) = 291.16
  heatprop(:) = (/ 0.5, 0.5, 0.5, 0.5, 1., 0., 0., 0. /)
  coolprop(:) = (/ 0.5, 0.5, 0.5, 0.5, 1., 0., 0., 0. /)
end if


! Define veg indices
if ( fname(10)/='' .and. outmode==1 ) then
   
  write(6,*) "Defining user specified VEG->PFT mapping"
  open(unit=40,file=fname(10),status='old',action='read',iostat=ioerror)
  if ( ioerror/=0 ) then
    write(6,*) "ERROR: Cannot open mapconfig file ",trim(fname(10))
    call finishbanner
    stop -1
  end if
  
  read(40,*) comments
  read(40,*) class_num
  allocate( mapindex(class_num,5), mapfrac(class_num,5) )
  allocate( mapwater(class_num), mapice(class_num) )
  allocate( mapjveg(class_num) )
  mapindex(:,:)=0
  mapfrac(:,:)=0. 
  mapwater(:)=.false.
  mapice(:)=.false.
  mapjveg(:)=0

  read(40,*) comments
  do i=1,class_num
    read(40,'(A)',iostat=ioerror) largestring
    if ( ioerror/=0 ) then
      write(6,*) "ERROR: Cannot read entry on mapconfig ",trim(fname(10))  
      write(6,*) "ioerror= ",ioerror
      call finishbanner
      stop -1
    end if

    iposend=0 ! must start with 0
    call findentry_integer(largestring,iposbeg,iposend,.false.,jveg)
    mapjveg(i) = jveg
    call findentry_character(largestring,iposbeg,iposend,.false.,jdesc)
    call findentry_logical(largestring,iposbeg,iposend,.false.,mapwater(i))
    call findentry_logical(largestring,iposbeg,iposend,.false.,mapice(i))
    maxindex = 0
    do j = 1,5
      call findentry_real(largestring,iposbeg,iposend,.true.,mapfrac(i,j))
      if ( iposbeg==-1 ) exit
      call findentry_character(largestring,iposbeg,iposend,.false.,kdesc)
      if ( iposbeg==-1 ) exit
      call findindex(kdesc,pft_desc,pft_len,ateb_desc,ateb_len,mapindex(i,j))
      maxindex=j
    end do
    
    if ( maxindex<1 ) then
      write(6,*) "ERROR: No valid PFTs or urban classes in mapconfig line"
      write(6,*) trim(largestring)
      call finishbanner
      stop -1
    end if
    
    write(6,*) "Processed ",trim(jdesc)," with ",maxindex," component out of 5"
    
  end do
  close(40)
   
else
    
  write(6,*) "Defining default VEG->PFT mapping"      
  class_num = 17
  allocate( mapindex(class_num,5), mapfrac(class_num,5) )
  allocate( mapwater(class_num), mapice(class_num) )
  allocate( mapjveg(class_num) )
  mapindex(:,:)=0
  mapfrac(:,:)=0. 
  mapwater(:)=.false.
  mapice(:)=.false.
  mapjveg(:)=0
  mapindex(1,1) = 1   ! Evergreen_needleaf
  mapfrac(1,1) = 1.
  mapjveg(1) = 1   
  mapindex(2,1) = 2   ! Evergreen_broadleaf
  mapfrac(2,1) = 1.
  mapjveg(2) = 2
  mapindex(3,1) = 3   ! Deciduous_needleaf
  mapfrac(3,1) = 1.
  mapjveg(3) = 3
  mapindex(4,1) = 4   ! Deciduous_broadleaf
  mapfrac(4,1) = 1.
  mapjveg(4) = 4  
  mapindex(5,1) = -1  ! Mixed forest - Mixed
  mapfrac(5,1) = 1.
  mapjveg(5) = 5  
  mapindex(6,1) = 5   ! Closed Shrublands - Shrub
  mapfrac(6,1) = 0.8
  mapindex(6,2) = -2  ! closed Shrublands - Grass
  mapfrac(6,2) = 0.2
  mapjveg(6) = 6  
  mapindex(7,1) = 5   ! Open Shrublands - Shrub
  mapfrac(7,1) = 0.2
  mapindex(7,2) = -2  ! open shrublands - grass
  mapfrac(7,2) = 0.8
  mapjveg(7) = 7  
  mapindex(8,1) = -2  ! woody savannas - grass
  mapfrac(8,1) = 0.6
  mapindex(8,2) = -5  ! woody savannas - needle/broad savanna
  mapfrac(8,2) = 0.4
  mapjveg(8) = 8  
  mapindex(9,1) = -2  ! savannas - grass
  mapfrac(9,1)= 0.9
  mapindex(9,2) = -5  ! savannas - needle/broad savanna
  mapfrac(9,2) = 0.1
  mapjveg(9) = 9    
  mapindex(10,1) = -2 ! grasslands - grass
  mapfrac(10,1) = 1.
  mapjveg(10) = 10
  mapindex(11,1) = 11 ! permanent wetlands - wetland
  mapfrac(11,1) = 1.
  mapjveg(11) = 11
  mapindex(12,1) = -4 ! croplands - crop
  mapfrac(12,1) = 1.
  mapjveg(12) = 12    
  mapindex(13,1) = -101 ! urban and built-up - urban
  mapfrac(13,1) = 1.
  mapjveg(13) = 13    
  mapindex(14,1) = -4 ! cropland/natural vegetation mosaic - crop
  mapfrac(14,1) = 1.
  mapjveg(14) = 14    
  mapindex(15,1) = 17 ! snow and ice - ice
  mapfrac(15,1) = 1.
  mapjveg(15) = 15    
  mapice(15) = .true.
  mapindex(16,1) = 14 ! barran or sparsely vegetated - barren
  mapfrac(16,1) = 1.
  mapjveg(16) = 16    
  mapindex(17,1) = 16 ! water bodies - lakes
  mapfrac(17,1) = 1.
  mapjveg(17) = 17    
  mapwater(17) = .true.
  
end if



! allocate memory
allocate( sermsk(class_num) )
allocate(albvisdata(sibdim(1),sibdim(2)))
allocate(albnirdata(sibdim(1),sibdim(2)))
allocate(soildata(sibdim(1),sibdim(2),0:8))
allocate(landdata(sibdim(1),sibdim(2),0:class_num*(1+mthrng)))

! Read default igbp data
call getdata(landdata,lonlat,gridout,rlld,sibdim,class_num*(1+mthrng),sibsize,'land',fastigbp,ozlaipatch,binlimit,month,year, &
             fname(4),fname(6),class_num,mapjveg,mapwater)
call getdata(soildata,lonlat,gridout,rlld,sibdim,8,sibsize,'soil',fastigbp,ozlaipatch,binlimit,month,year, &
             fname(5),fname(6),class_num,mapjveg,mapwater)
call getdata(albvisdata,lonlat,gridout,rlld,sibdim,0,sibsize,'albvis',fastigbp,ozlaipatch,binlimit,month,year, &
             fname(7),fname(6),class_num,mapjveg,mapwater)
call getdata(albnirdata,lonlat,gridout,rlld,sibdim,0,sibsize,'albnir',fastigbp,ozlaipatch,binlimit,month,year, &
             fname(8),fname(6),class_num,mapjveg,mapwater)

! Read land-use change data
if ( fname(15)/='' ) then
  if ( outmode/=1 ) then
    write(6,*) "ERROR: Change in land-use requires outmode=1"
    call finishbanner
    stop -1
  end if    
  if ( .not.tile ) then
    write(6,*) "ERROR: Change in land-use requires tile=.true."
    call finishbanner
    stop -1
  end if
  write(6,*) "Applying land-use change data from ",trim(fname(15))
  allocate( changedata(sibdim(1),sibdim(2),0:1) ) 
  ! read land-use change dataset
  call getdata(changedata,lonlat,gridout,rlld,sibdim,1,sibsize,'change',fastigbp,ozlaipatch,binlimit,month,year, &
               fname(15),fname(6),class_num,mapjveg,mapwater)
  ! reduce natural vegetation and add land-use changes
  
  do j = 1,sibdim(2)
    do i = 1,sibdim(1)
      nsum = sum(landdata(i,j,1:class_num),mask=.not.mapwater(1:class_num))
      if ( nsum>0. ) then
        change_crop = changedata(i,j,0)
        change_pasture = max( changedata(i,j,1), landdata(i,j,10) )
        landdata(i,j,12) = 0. ! IGBP crops=12
        landdata(i,j,14) = 0. ! IGBP crops/natural vegetation mosaic=14
        landdata(i,j,10) = 0. ! IGBP grassland=10 
        newsum = sum(landdata(i,j,1:class_num),mask=.not.mapwater(1:class_num)) 
        where ( .not.mapwater(1:class_num) )
          landdata(i,j,1:class_num) = landdata(i,j,1:class_num)*max(1.-change_crop-change_pasture,0.001)*nsum/newsum
        end where  
        landdata(i,j,12) = max(landdata(i,j,12) + change_crop*nsum,1.e-3)
        landdata(i,j,10) = max(landdata(i,j,10) + change_pasture*nsum,1.e-3)
      end if  
    end do
  end do  
  deallocate( changedata )
end if

! Read user defined data
if ( fname(11)/='' .or. fname(12)/='' ) then
  call modifylanddata(landdata,lonlat,sibdim,class_num*(1+mthrng),month,fname(11),fname(12),class_num,mapjveg,gridout,ovegfrac)
end if

deallocate(gridout)
allocate(urbandata(sibdim(1),sibdim(2)),lsdata(sibdim(1),sibdim(2)),oceandata(sibdim(1),sibdim(2)))
allocate(urbantype(sibdim(1),sibdim(2)))
allocate(testdata(sibdim(1),sibdim(2)))

write(6,*) "Preparing data..."
! extract urban cover and remove from landdata
urbantype(:,:)=1
urbandata(:,:)=0.
testdata(:,:)=0.
do i = 1,class_num
    
  urbanmaxfrac = 0.
  urbantotalfrac = 0.
  do j = 1,5
    if ( mapindex(i,j)<-100 .and. mapindex(i,j)>-200 ) then
      urbandata(:,:) = urbandata(:,:) + landdata(:,:,i)*mapfrac(i,j)
      urbantotalfrac = urbantotalfrac + mapfrac(i,j)
      if ( mapfrac(i,j)>urbanmaxfrac ) then
        urbanmaxfrac = mapfrac(i,j)
        where( landdata(:,:,i)*mapfrac(i,j)>testdata(:,:) )
          urbantype(:,:) = -mapindex(i,j)-100
          testdata(:,:) = landdata(:,:,i)*mapfrac(i,j)
        end where
      end if
    end if
  end do
  if ( urbantotalfrac>0.999 ) then
    landdata(:,:,i) = 0. ! remove 100% urban classes
  end if
end do

call igbpfix(landdata,rlld,sibdim,class_num,mthrng,mapwater)

if ( igbplsmask ) then
  write(6,*) "Using IGBP land/sea mask"
  testdata(:,:) = landdata(:,:,0)
  do i = 1,class_num
    if ( mapwater(i) ) then
      testdata(:,:) = testdata(:,:) + landdata(:,:,i)
    end if
  end do
  where ( testdata(:,:)>0. )
    oceandata=landdata(:,:,0)/testdata(:,:)
  elsewhere
    oceandata=0.
  end where
  lsdata=real(nint(testdata(:,:)))
  call cleantopo(tunit,fname(1),fname(3),lsdata,oceandata,sibdim,zerozs)
else
  write(6,*) "Using topography land/sea mask"
  call gettopols(tunit,fname(1),lsdata,sibdim)
end if

deallocate(oceandata)
allocate(idata(sibdim(1),sibdim(2)),tmp(sibdim(1),sibdim(2),0:1))
allocate(rdata(sibdim(1),sibdim(2)))

write(6,*) "Clean urban data"
urbandata=min(urbandata,(1.-lsdata))

! Clean-up soil, lai, veg, albedo and urban data
write(6,*) "Clean landuse data"
call cleanigbp(landdata,lsdata,rlld,sibdim,class_num,mthrng,mapwater)
write(6,*) "Clean soil data"
call cleanreal(soildata,8,lsdata,rlld,sibdim)
write(6,*) "Calculate soil texture"
call calsoilnear(landdata,soildata,lsdata,sibdim,idata,class_num,mapwater,mapice)
write(6,*) "Clean albedo data"
where (lsdata>=0.5)
  albvisdata(:,:)=0.08 ! 0.07 in Masson (2003)
  albnirdata(:,:)=0.08 ! 0.20 in Masson (2003)
elsewhere (idata==9)
  albvisdata(:,:)=0.80
  albnirdata(:,:)=0.40
end where
tmp(:,:,0)=albvisdata
tmp(:,:,1)=albnirdata
call cleanreal(tmp,1,lsdata,rlld,sibdim)
albvisdata=tmp(:,:,0)
albnirdata=tmp(:,:,1)

deallocate( soildata, tmp )
allocate( vfrac(sibdim(1),sibdim(2),9), vtype(sibdim(1),sibdim(2),9) )
allocate( vlai(sibdim(1),sibdim(2),9) )
!allocate( savannafrac(sibdim(1),sibdim(2)) )

write(6,*) "Create output file"
dimnum(1:2)=sibdim(1:2) ! CC grid dimensions
dimnum(3)=1 ! Turn off level
dimnum(4)=1 ! Number of months in a year
adate=0 ! Turn off date
adate(2)=1 ! time units=months


! Prep nc output
do tt=1,mthrng
  write(6,*) "Writing month ",tt,"/",mthrng

  if ( fname(15)/='' ) then
    if (mthrng==1) then
      filename=fname(2)
    else
      write(filename,"(A,'.',I4.4,'.',I2.2)") trim(fname(2)),year,tt
    end if
  else    
    if (mthrng==1) then
      filename=fname(2)
    else
      write(filename,"(A,'.',I2.2)") trim(fname(2)),tt
    end if
  end if  

  call ncinitcc(ncidarr,filename,dimnum(1:3),dimid,adate)
  if ( outmode==1 ) then
    call ncadd_dimension(ncidarr,'pft',pft_len,pft_dimid)
    call ncadd_dimension(ncidarr,'ateb',ateb_len,ateb_dimid)
    call ncadd_dimension(ncidarr,'soil',soil_len,soil_dimid)
  end if
  outputdesc(1)='soilt'
  outputdesc(2)='Soil classification'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(2),1.,0.)
  outputdesc(1)='albvis'
  outputdesc(2)='Soil albedo (VIS)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(3),1.,0.)
  outputdesc(1)='albnir'
  outputdesc(2)='Soil albedo (NIR)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(4),1.,0.)
  
  outputdesc(1)='lai1'
  outputdesc(2)='Leaf Area Index (tile1)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(5),1.,0.)
  outputdesc(1)='vegt1'
  outputdesc(2)='Land-use classification (tile1)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(6),1.,0.)
  outputdesc(1)='vfrac1'
  outputdesc(2)='Land-use cover fraction (tile1)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(7),1.,0.)

  outputdesc(1)='lai2'
  outputdesc(2)='Leaf Area Index (tile2)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(8),1.,0.)
  outputdesc(1)='vegt2'
  outputdesc(2)='Land-use classification (tile2)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(9),1.,0.)
  outputdesc(1)='vfrac2'
  outputdesc(2)='Land-use cover fraction (tile2)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(10),1.,0.)

  outputdesc(1)='lai3'
  outputdesc(2)='Leaf Area Index (tile3)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(11),1.,0.)
  outputdesc(1)='vegt3'
  outputdesc(2)='Land-use classification (tile3)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(12),1.,0.)
  outputdesc(1)='vfrac3'
  outputdesc(2)='Land-use cover fraction (tile3)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(13),1.,0.)

  outputdesc(1)='lai4'
  outputdesc(2)='Leaf Area Index (tile4)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(14),1.,0.)
  outputdesc(1)='vegt4'
  outputdesc(2)='Land-use classification (tile4)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(15),1.,0.)
  outputdesc(1)='vfrac4'
  outputdesc(2)='Land-use cover fraction (tile4)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(16),1.,0.)

  outputdesc(1)='lai5'
  outputdesc(2)='Leaf Area Index (tile5)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(17),1.,0.)
  outputdesc(1)='vegt5'
  outputdesc(2)='Land-use classification (tile5)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(18),1.,0.)
  outputdesc(1)='vfrac5'
  outputdesc(2)='Land-use cover fraction (tile5)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(19),1.,0.)

  outputdesc(1)='lai6'
  outputdesc(2)='Leaf Area Index (tile6)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(20),1.,0.)
  outputdesc(1)='vegt6'
  outputdesc(2)='Land-use classification (tile6)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(21),1.,0.)
  outputdesc(1)='vfrac6'
  outputdesc(2)='Land-use cover fraction (tile6)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(22),1.,0.)  
  
  outputdesc(1)='lai7'
  outputdesc(2)='Leaf Area Index (tile7)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(23),1.,0.)
  outputdesc(1)='vegt7'
  outputdesc(2)='Land-use classification (tile7)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(24),1.,0.)
  outputdesc(1)='vfrac7'
  outputdesc(2)='Land-use cover fraction (tile7)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(25),1.,0.) 
  
  outputdesc(1)='lai8'
  outputdesc(2)='Leaf Area Index (tile8)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(26),1.,0.)
  outputdesc(1)='vegt8'
  outputdesc(2)='Land-use classification (tile8)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(27),1.,0.)
  outputdesc(1)='vfrac8'
  outputdesc(2)='Land-use cover fraction (tile8)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(28),1.,0.) 
  
  outputdesc(1)='lai9'
  outputdesc(2)='Leaf Area Index (tile9)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(29),1.,0.)
  outputdesc(1)='vegt9'
  outputdesc(2)='Land-use classification (tile9)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(30),1.,0.)
  outputdesc(1)='vfrac9'
  outputdesc(2)='Land-use cover fraction (tile9)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(31),1.,0.) 
  
  outputdesc(1)='urbantype'
  outputdesc(2)='Urban class'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(32),1.,0.)
  outputdesc(1)='urban'
  outputdesc(2)='Urban fraction'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(33),1.,0.)

  ! to be depreciated
  !outputdesc(1)='savanna'
  !outputdesc(2)='Savanna fraction of PFT=2'
  !outputdesc(3)='none'
  !call ncaddvargen(ncidarr,outputdesc,5,2,varid(34),1.,0.)
  
  call ncatt(ncidarr,'lon0',lonlat(1))
  call ncatt(ncidarr,'lat0',lonlat(2))
  call ncatt(ncidarr,'schmidt',schmidt)
  call ncatt(ncidarr,'cableversion',3939.) ! CABLE version for data
  if ( outmode==1 ) then
    call ncatt(ncidarr,'cableformat',1.)
    call ncatt(ncidarr,'atebformat',3.)
    call ncatt(ncidarr,'soilformat',1.)
  else
    call ncatt(ncidarr,'cableformat',0.)
    call ncatt(ncidarr,'atebformat',0.)
    call ncatt(ncidarr,'soilformat',0.)
  end if

  ! PFT metadata
  if ( outmode==1 ) then
    outputdesc(1)='pftname'
    outputdesc(2)='PFT description'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,2,pft_dimid)
    outputdesc(1)='csiropft'
    outputdesc(2)='CSIRO PFT index'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='hc'
    outputdesc(2)='Canopy height'
    outputdesc(3)='m'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='xfang'
    outputdesc(2)='Leaf angle'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='leaf_w'
    outputdesc(2)='Leaf width'
    outputdesc(3)='m'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='leaf_l'
    outputdesc(2)='Leaf length'
    outputdesc(3)='m'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='canst1'
    outputdesc(2)='Canopy water storage'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='shelrb'
    outputdesc(2)='shelrb'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='extkn'
    outputdesc(2)='extkn'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='rholeaf-vis'
    outputdesc(2)='Leaf reflection VIS'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='rholeaf-nir'
    outputdesc(2)='Leaf reflection NIR'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='tauleaf-vis'
    outputdesc(2)='Leaf transmission VIS'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='tauleaf-nir'
    outputdesc(2)='Leaf transmission NIR'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='vcmax'
    outputdesc(2)='vcmax'
    outputdesc(3)='mol/m2/s'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='rpcoef'
    outputdesc(2)='rpcoef'
    outputdesc(3)='1/degC'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='rootbeta'
    outputdesc(2)='rootbeta'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='c4frac'
    outputdesc(2)='C4 fraction'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='vbeta'
    outputdesc(2)='vbeta'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='a1gs'
    outputdesc(2)='a1'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='d0gs'
    outputdesc(2)='d0'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='alpha'
    outputdesc(2)='alpha'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='convex'
    outputdesc(2)='convex'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='cfrd'
    outputdesc(2)='cfrd'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='gswmin'
    outputdesc(2)='gswmin'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='conkc0'
    outputdesc(2)='conkc0'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='conko0'
    outputdesc(2)='conko0'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='ekc'
    outputdesc(2)='ekc'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='eko'
    outputdesc(2)='eko'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='g0'
    outputdesc(2)='g0'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='g1'
    outputdesc(2)='g1'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='zr'
    outputdesc(2)='zr'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    outputdesc(1)='clitt'
    outputdesc(2)='clitt'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,pft_dimid)
    
    outputdesc(1)='atebname'
    outputdesc(2)='ATEB description'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,2,ateb_dimid)
    outputdesc(1)='bldheight'
    outputdesc(2)='Building height'
    outputdesc(3)='m'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='hwratio'
    outputdesc(2)='Building height to width ratio'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='sigvegc'
    outputdesc(2)='Canyon vegetation area fraction'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='sigmabld'
    outputdesc(2)='Building area fraction'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='industryfg'
    outputdesc(2)='Industral heat flux'
    outputdesc(3)='W/m2'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='trafficfg'
    outputdesc(2)='Traffic heat flux'
    outputdesc(3)='W/m2'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='roofalpha'
    outputdesc(2)='Roof albedo'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='wallalpha'
    outputdesc(2)='Wall albedo'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='roadalpha'
    outputdesc(2)='Road albedo'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='vegalphac'
    outputdesc(2)='Canyon vegetation albedo'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='zovegc'
    outputdesc(2)='Canyon vegetation roughness length'
    outputdesc(3)='m'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    do i = 1,4
      write(outputdesc(1),'("roof_thick_l",(I1.1))') i
      write(outputdesc(2),'("Roof layer ",(I1.1)," thickness")') i
      outputdesc(3)='m'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("roof_cp_l",(I1.1))') i
      write(outputdesc(2),'("Roof layer ",(I1.1)," heat capacity")') i
      outputdesc(3)='J/m3/K'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("roof_cond_l",(I1.1))') i
      write(outputdesc(2),'("Roof layer ",(I1.1)," heat conductivity")') i
      outputdesc(3)='W/m/K'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("wall_thick_l",(I1.1))') i
      write(outputdesc(2),'("Wall layer ",(I1.1)," thickness")') i
      outputdesc(3)='m'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("wall_cp_l",(I1.1))') i
      write(outputdesc(2),'("Wall layer ",(I1.1)," heat capacity")') i
      outputdesc(3)='J/m3/K'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("wall_cond_l",(I1.1))') i
      write(outputdesc(2),'("Wall layer ",(I1.1)," heat conductivity")') i
      outputdesc(3)='W/m/K'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("slab_thick_l",(I1.1))') i
      write(outputdesc(2),'("Slab layer ",(I1.1)," thickness")') i
      outputdesc(3)='m'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("slab_cp_l",(I1.1))') i
      write(outputdesc(2),'("Slab layer ",(I1.1)," heat capacity")') i
      outputdesc(3)='J/m3/K'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("slab_cond_l",(I1.1))') i
      write(outputdesc(2),'("Slab layer ",(I1.1)," heat conductivity")') i
      outputdesc(3)='W/m/K'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("road_thick_l",(I1.1))') i
      write(outputdesc(2),'("Road layer ",(I1.1)," thickness")') i
      outputdesc(3)='m'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("road_cp_l",(I1.1))') i
      write(outputdesc(2),'("Road layer ",(I1.1)," heat capacity")') i
      outputdesc(3)='J/m3/K'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
      write(outputdesc(1),'("road_cond_l",(I1.1))') i
      write(outputdesc(2),'("Road layer ",(I1.1)," heat conductivity")') i
      outputdesc(3)='W/m/K'
      call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    end do  
    outputdesc(1)='infiltration'
    outputdesc(2)='Infiltration air volume changes per hour'
    outputdesc(3)='m3/m3'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='internalgain'
    outputdesc(2)='Internal gains sensible heat flux'
    outputdesc(3)='W/m2'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='bldtemp'
    outputdesc(2)='Comfort temperature'
    outputdesc(3)='K'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='heatprop'
    outputdesc(2)='Fraction of spaces with heating devices'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='coolprop'
    outputdesc(2)='Fraction of spaces with cooling devices'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,ateb_dimid)
    outputdesc(1)='soilname'
    outputdesc(2)='Soil type description'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,2,soil_dimid)
    outputdesc(1)='silt'
    outputdesc(2)='Silt fraction of soil'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='clay'
    outputdesc(2)='Clay fraction of soil'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='sand'
    outputdesc(2)='Sand fraction of soil'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='swilt'
    outputdesc(2)='H2O volume at wilting'
    outputdesc(3)='m3'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='sfc'
    outputdesc(2)='H2O volume at field capacity'
    outputdesc(3)='m3'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='ssat'
    outputdesc(2)='H2O volume at saturation'
    outputdesc(3)='m3'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='bch'
    outputdesc(2)='Parameter b in Campbell equation'
    outputdesc(3)='none'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='hyds'
    outputdesc(2)='Hydraulic conductivity at saturation'
    outputdesc(3)='m/s'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='sucs'
    outputdesc(2)='Suction at saturation'
    outputdesc(3)='m'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='rhosoil'
    outputdesc(2)='Soil density'
    outputdesc(3)='kg/m3'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
    outputdesc(1)='css'
    outputdesc(2)='Specific heat capacity of soil'
    outputdesc(3)='kJ/kg/K'
    call ncadd_1dvar(ncidarr,outputdesc,5,soil_dimid)
  end if

  call ncenddef(ncidarr)
  alonlat(:,1)=(/ 1., real(sibdim(1)), 1. /)
  alonlat(:,2)=(/ 1., real(sibdim(2)), 1. /)
  alvl=1.
  if (mthrng==12) then
    atime(1)=Real(tt) ! Define Months
  else
    atime(1)=real(month)
  end if
  call nclonlatgen(ncidarr,dimid,alonlat,alvl,atime,dimnum)


  ! Write soil type
  write(6,*) 'Write soil type file.'
  dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
  rdata=real(idata)
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(2))

  ! Write albedo file
  write(6,*) 'Write albedo files.'
  dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
  call ncwritedatgen(ncidarr,albvisdata,dimcount,varid(3))

  write(formout,'("(",i3,"f4.0)" )') sibdim(1)
  dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
  call ncwritedatgen(ncidarr,albnirdata,dimcount,varid(4))

  write(6,*) 'Write land-use'
  vtype = 0
  vfrac = 0.
  vlai = 0.
  do j=1,sibdim(2)
    do i=1,sibdim(1)
      if (lsdata(i,j)>=0.5) then
        ! water  
        vtype(i,j,1:9)=0
        vfrac(i,j,1)=1.
        vfrac(i,j,2:9)=0.
        vlai(i,j,:)=0.
      else
        ! land  
        if ( .not. tile ) then
          sermsk=.not.mapwater(:)  
          sibmax=maxloc(landdata(i,j,1:class_num),sermsk)
          vtype(i,j,1)=sibmax(1)
          vfrac(i,j,1)=landdata(i,j,sibmax(1))
          vlai(i,j,1)=landdata(i,j,class_num+(sibmax(1)-1)*mthrng+tt)    
        else    
          sermsk=.not.mapwater(:)
          sermsk(10) = .false.
          sermsk(12) = .false.
          icefrac = 0.
          do k = 1,5 ! only for natural vegetation
            sibmax=maxloc(landdata(i,j,1:class_num),sermsk)
            sermsk(sibmax(1))=.false.
            vtype(i,j,k)=sibmax(1)
            vfrac(i,j,k)=landdata(i,j,sibmax(1))
            vlai(i,j,k)=landdata(i,j,class_num+(sibmax(1)-1)*mthrng+tt)
            if ( mapice(sibmax(1)) ) then
              icefrac = icefrac + vfrac(i,j,k)
            end if
          end do
          if ( icefrac<0.99 ) then ! avoid crops where there is large amounts of ice
            k = 6 ! grassland
            sibmax(1) = 10
            vtype(i,j,k)=sibmax(1)
            vfrac(i,j,k)=max( landdata(i,j,sibmax(1)), 0.001 )
            vlai(i,j,k)=landdata(i,j,class_num+(sibmax(1)-1)*mthrng+tt)
            k = 7 ! crop
            sibmax(1) = 12
            vtype(i,j,k)=sibmax(1)
            vfrac(i,j,k)=max( landdata(i,j,sibmax(1)), 0.001 )
            vlai(i,j,k)=landdata(i,j,class_num+(sibmax(1)-1)*mthrng+tt)
          end if  
          ! k = 8,9 are reserved for outmode=1
          vfrac(i,j,:)=vfrac(i,j,:)/sum(vfrac(i,j,:))
          !vfrac(i,j,1:4)=real(nint(100.*vfrac(i,j,1:4)))/100.
          !vfrac(i,j,5)=1.-sum(vfrac(i,j,1:4))
          !do k = 5,2,-1
          !  if ((vfrac(i,j,k)<0.).or.(vfrac(i,j,k-1)<vfrac(i,j,k))) then
          !    vfrac(i,j,k-1)=vfrac(i,j,k-1)+vfrac(i,j,k)
          !    vfrac(i,j,k)=0.
          !  end if
          !end do
        end if  
      end if
    end do
  end do
  if ( outmode==1 ) then
    call convertigbp(vtype,vfrac,vlai,sibdim,lsdata,rlld,class_num,mapindex,mapfrac,pft_len)
  end if
  dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
  rdata=Real(vtype(:,:,1))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(6))
  rdata=Real(vtype(:,:,2))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(9))
  rdata=Real(vtype(:,:,3))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(12))
  rdata=Real(vtype(:,:,4))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(15))
  rdata=Real(vtype(:,:,5))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(18))
  rdata=Real(vtype(:,:,6))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(21))
  rdata=Real(vtype(:,:,7))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(24))
  rdata=Real(vtype(:,:,8))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(27))
  rdata=Real(vtype(:,:,9))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(30))
  call ncwritedatgen(ncidarr,vfrac(:,:,1),dimcount,varid(7))
  call ncwritedatgen(ncidarr,vfrac(:,:,2),dimcount,varid(10))
  call ncwritedatgen(ncidarr,vfrac(:,:,3),dimcount,varid(13))
  call ncwritedatgen(ncidarr,vfrac(:,:,4),dimcount,varid(16))
  call ncwritedatgen(ncidarr,vfrac(:,:,5),dimcount,varid(19))
  call ncwritedatgen(ncidarr,vfrac(:,:,6),dimcount,varid(22))
  call ncwritedatgen(ncidarr,vfrac(:,:,7),dimcount,varid(25))
  call ncwritedatgen(ncidarr,vfrac(:,:,8),dimcount,varid(28))
  call ncwritedatgen(ncidarr,vfrac(:,:,9),dimcount,varid(31))
  call ncwritedatgen(ncidarr,vlai(:,:,1),dimcount,varid(5))
  call ncwritedatgen(ncidarr,vlai(:,:,2),dimcount,varid(8))
  call ncwritedatgen(ncidarr,vlai(:,:,3),dimcount,varid(11))
  call ncwritedatgen(ncidarr,vlai(:,:,4),dimcount,varid(14))
  call ncwritedatgen(ncidarr,vlai(:,:,5),dimcount,varid(17))
  call ncwritedatgen(ncidarr,vlai(:,:,6),dimcount,varid(20))
  call ncwritedatgen(ncidarr,vlai(:,:,7),dimcount,varid(23))
  call ncwritedatgen(ncidarr,vlai(:,:,8),dimcount,varid(26))
  call ncwritedatgen(ncidarr,vlai(:,:,9),dimcount,varid(29))

  ! Urban
  write(6,*) 'Write urban'
  dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
  ! type
  where (urbandata>0.) 
    rdata=real(urbantype)
  elsewhere
    rdata=0.
  end where
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(32))
  ! fraction
  urbanfrac=1.
  rdata=urbandata*urbanfrac
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(33))
  
  if ( outmode==1 ) then
    call ncput_1dvar_text(ncidarr,'pftname',pft_len,pft_desc)
    call ncput_1dvar_real(ncidarr,'csiropft',pft_len,csiropft)
    call ncput_1dvar_real(ncidarr,'hc',pft_len,hc)
    call ncput_1dvar_real(ncidarr,'xfang',pft_len,xfang)
    call ncput_1dvar_real(ncidarr,'leaf_w',pft_len,leaf_w)
    call ncput_1dvar_real(ncidarr,'leaf_l',pft_len,leaf_l)
    call ncput_1dvar_real(ncidarr,'canst1',pft_len,canst1)
    call ncput_1dvar_real(ncidarr,'shelrb',pft_len,shelrb)
    call ncput_1dvar_real(ncidarr,'extkn',pft_len,extkn)
    call ncput_1dvar_real(ncidarr,'rholeaf-vis',pft_len,refl(:,1))
    call ncput_1dvar_real(ncidarr,'rholeaf-nir',pft_len,refl(:,2))
    call ncput_1dvar_real(ncidarr,'tauleaf-vis',pft_len,taul(:,1))
    call ncput_1dvar_real(ncidarr,'tauleaf-nir',pft_len,taul(:,2))
    call ncput_1dvar_real(ncidarr,'vcmax',pft_len,vcmax)
    call ncput_1dvar_real(ncidarr,'rpcoef',pft_len,rpcoef)
    call ncput_1dvar_real(ncidarr,'rootbeta',pft_len,rootbeta)
    call ncput_1dvar_real(ncidarr,'c4frac',pft_len,c4frac)
    call ncput_1dvar_real(ncidarr,'vbeta',pft_len,vbeta)
    call ncput_1dvar_real(ncidarr,'a1gs',pft_len,a1gs)
    call ncput_1dvar_real(ncidarr,'d0gs',pft_len,d0gs)
    call ncput_1dvar_real(ncidarr,'alpha',pft_len,alpha)
    call ncput_1dvar_real(ncidarr,'convex',pft_len,convex)
    call ncput_1dvar_real(ncidarr,'cfrd',pft_len,cfrd)
    call ncput_1dvar_real(ncidarr,'gswmin',pft_len,gswmin)
    call ncput_1dvar_real(ncidarr,'conkc0',pft_len,conkc0)
    call ncput_1dvar_real(ncidarr,'conko0',pft_len,conko0)
    call ncput_1dvar_real(ncidarr,'ekc',pft_len,ekc)
    call ncput_1dvar_real(ncidarr,'eko',pft_len,eko)
    call ncput_1dvar_real(ncidarr,'g0',pft_len,g0)
    call ncput_1dvar_real(ncidarr,'g1',pft_len,g1)
    call ncput_1dvar_real(ncidarr,'zr',pft_len,zr)
    call ncput_1dvar_real(ncidarr,'clitt',pft_len,clitt)
    
    call ncput_1dvar_text(ncidarr,'atebname',ateb_len,ateb_desc)
    call ncput_1dvar_real(ncidarr,'bldheight',ateb_len,bldheight)
    call ncput_1dvar_real(ncidarr,'hwratio',ateb_len,hwratio)
    call ncput_1dvar_real(ncidarr,'sigvegc',ateb_len,sigvegc)
    call ncput_1dvar_real(ncidarr,'sigmabld',ateb_len,sigmabld)
    call ncput_1dvar_real(ncidarr,'industryfg',ateb_len,industryfg)
    call ncput_1dvar_real(ncidarr,'trafficfg',ateb_len,trafficfg)
    call ncput_1dvar_real(ncidarr,'roofalpha',ateb_len,roofalpha)
    call ncput_1dvar_real(ncidarr,'wallalpha',ateb_len,wallalpha)
    call ncput_1dvar_real(ncidarr,'roadalpha',ateb_len,roadalpha)
    call ncput_1dvar_real(ncidarr,'vegalphac',ateb_len,vegalphac)
    call ncput_1dvar_real(ncidarr,'zovegc',ateb_len,zovegc)
    do i = 1,4
      write(vname,'("roof_thick_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,roofthick(:,i))
      write(vname,'("roof_cp_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,roofcp(:,i))
      write(vname,'("roof_cond_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,roofcond(:,i))
      write(vname,'("wall_thick_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,wallthick(:,i))
      write(vname,'("wall_cp_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,wallcp(:,i))
      write(vname,'("wall_cond_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,wallcond(:,i))
      write(vname,'("slab_thick_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,slabthick(:,i))
      write(vname,'("slab_cp_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,slabcp(:,i))
      write(vname,'("slab_cond_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,slabcond(:,i))
      write(vname,'("road_thick_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,roadthick(:,i))
      write(vname,'("road_cp_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,roadcp(:,i))
      write(vname,'("road_cond_l",(I1.1))') i  
      call ncput_1dvar_real(ncidarr,vname,ateb_len,roadcond(:,i))
    end do  
    call ncput_1dvar_real(ncidarr,'infiltration',ateb_len,infiltration)
    call ncput_1dvar_real(ncidarr,'internalgain',ateb_len,internalgain)
    call ncput_1dvar_real(ncidarr,'bldtemp',ateb_len,bldtemp)
    call ncput_1dvar_real(ncidarr,'heatprop',ateb_len,heatprop)
    call ncput_1dvar_real(ncidarr,'coolprop',ateb_len,coolprop)
    call ncput_1dvar_text(ncidarr,'soilname',soil_len,soil_desc)
    call ncput_1dvar_real(ncidarr,'silt',soil_len,silt)
    call ncput_1dvar_real(ncidarr,'clay',soil_len,clay)
    call ncput_1dvar_real(ncidarr,'sand',soil_len,sand)
    call ncput_1dvar_real(ncidarr,'swilt',soil_len,swilt)
    call ncput_1dvar_real(ncidarr,'sfc',soil_len,sfc)
    call ncput_1dvar_real(ncidarr,'ssat',soil_len,ssat)
    call ncput_1dvar_real(ncidarr,'bch',soil_len,bch)
    call ncput_1dvar_real(ncidarr,'hyds',soil_len,hyds)
    call ncput_1dvar_real(ncidarr,'sucs',soil_len,sucs)
    call ncput_1dvar_real(ncidarr,'rhosoil',soil_len,rhosoil)
    call ncput_1dvar_real(ncidarr,'css',soil_len,css)
  end if
  
  call ncclose(ncidarr)

end do

deallocate( pft_desc, csiropft, hc, xfang, leaf_w, leaf_l )
deallocate( canst1, shelrb, extkn, refl, taul, vcmax )
deallocate( rpcoef, rootbeta, c4frac, vbeta )
deallocate( a1gs, d0gs, alpha, convex, cfrd )
deallocate( gswmin, conkc0, conko0, ekc, eko, g0, g1 )
deallocate( zr, clitt )

deallocate(landdata,urbandata,lsdata)
deallocate(vfrac,vtype,idata,vlai)
deallocate(testdata)
!deallocate(savannafrac)
deallocate(rlld)
deallocate(rdata)

deallocate( mapindex, mapfrac, mapwater, mapice )
deallocate( mapjveg )
deallocate( sermsk )

deallocate( ateb_desc )
deallocate( bldheight, hwratio, sigvegc, sigmabld )
deallocate( industryfg, trafficfg, roofalpha )
deallocate( wallalpha, roadalpha, vegalphac, zovegc )
deallocate( infiltration, internalgain, bldtemp )
deallocate( heatprop, coolprop )
deallocate( roofthick, roofcp, roofcond )
deallocate( wallthick, wallcp, wallcond )
deallocate( slabthick, slabcp, slabcond )
deallocate( roadthick, roadcp, roadcond )

if ( outmode==1 ) then
  deallocate( silt, clay, sand, swilt, sfc, ssat )
  deallocate( bch, hyds, sucs, rhosoil, css )
end if

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Fix IGBP data
!

subroutine igbpfix(landdata,rlld,sibdim,class_num,mthrng,mapwater)

implicit none

integer, intent(in) :: class_num, mthrng
integer, dimension(1:2), intent(in) :: sibdim
real, dimension(sibdim(1),sibdim(2),1:2), intent(in) :: rlld
real, dimension(sibdim(1),sibdim(2),0:class_num*(1+mthrng)), intent(inout) :: landdata
real, dimension(sibdim(1),sibdim(2),1:class_num*(1+mthrng)) :: newdata
logical, dimension(1:sibdim(1),1:sibdim(2)) :: allmsk, reqmsk
logical, dimension(class_num), intent(in) :: mapwater
integer i,j,ilon,ilat,k
real nsum,wsum

allmsk=.false.
do i=1,class_num
  if ( .not.mapwater(i) ) then
    allmsk=allmsk.or.landdata(:,:,i)>0.
  end if
end do
if (.not.any(allmsk)) return

reqmsk=.false.
do ilat=1,sibdim(2)
  do ilon=1,sibdim(1)
    wsum=landdata(ilon,ilat,0)+sum(landdata(ilon,ilat,1:class_num),mapwater) ! water
    if ( wsum<1. ) then
      reqmsk(ilon,ilat)=.true.
    end if
  end do
end do

!$OMP PARALLEL DO SCHEDULE(static) DEFAULT(NONE) SHARED(mapwater,newdata,sibdim,allmsk,reqmsk,mthrng,class_num,landdata) PRIVATE(i,j,k)
do j = 1,class_num*(1+mthrng)
  if ( j<=class_num ) then
    i = j
    k = 0
  else
    i = (j-class_num-1)/mthrng + 1
    k = mod(j-class_num-1,mthrng) + 1
  end if
  if ( .not.mapwater(i) ) then
    write(6,*) "Fill class ",j,i,k
    newdata(:,:,j) = landdata(:,:,j)
    call fill_cc_mask(newdata(:,:,j),sibdim(1),allmsk,reqmsk) ! only need land points to be filled
  else
    newdata(:,:,j) = 0. 
  end if
end do  
!$OMP END PARALLEL DO

do ilat=1,sibdim(2)
  do ilon=1,sibdim(1)
    wsum=landdata(ilon,ilat,0)+sum(landdata(ilon,ilat,1:class_num),mapwater) ! water
    if (wsum<1.) then
      if (.not.allmsk(ilon,ilat)) then
        do i=1,class_num
          if ( .not.mapwater(i) ) then
            landdata(ilon,ilat,i)=newdata(ilon,ilat,i)  
            landdata(ilon,ilat,(i-1)*mthrng+class_num+1:i*mthrng+class_num) &
                =newdata(ilon,ilat,(i-1)*mthrng+class_num+1:i*mthrng+class_num)
          end if
        end do
      end if
      nsum=sum(landdata(ilon,ilat,1:class_num),.not.mapwater) ! land
      do i=1,class_num
        if ( .not.mapwater(i) ) then
          landdata(ilon,ilat,i)=landdata(ilon,ilat,i)*max(1.-wsum,0.)/nsum
        end if
      end do
    end if
  end do
  if ( mod(ilat,100)==0 .or. ilat==sibdim(2) ) then
    write(6,*) "Searching ",ilat,"/",sibdim(2)
  end if
end do

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! clean land data
!

subroutine cleanigbp(dataout,lsdata,rlld,sibdim,class_num,mthrng,mapwater)

implicit none

integer, intent(in) :: class_num, mthrng
integer, dimension(2), intent(in) :: sibdim
real, dimension(sibdim(1),sibdim(2),0:class_num*(1+mthrng)), intent(inout) :: dataout
real, dimension(sibdim(1),sibdim(2)), intent(in) :: lsdata
real, dimension(sibdim(1),sibdim(2),2), intent(in) :: rlld
real, dimension(sibdim(1),sibdim(2),0:class_num*(1+mthrng)) :: datain
real, dimension(sibdim(1),sibdim(2)) :: testdata
logical, dimension(sibdim(1),sibdim(2)) :: sermsk,ocnmsk
logical, dimension(class_num), intent(in) :: mapwater
integer ilon,ilat,pxy(2),i
real nsum,wsum

datain=dataout
testdata(:,:)=0.
do i=1,class_num
  if ( .not.mapwater(i) ) then
    testdata(:,:) = testdata(:,:) + datain(:,:,i)
  end if
end do
sermsk=testdata(:,:)>0.
testdata(:,:)=datain(:,:,0)
do i=1,class_num
  if ( mapwater(i) ) then
    testdata(:,:) = testdata(:,:) + datain(:,:,i)
  end if
end do
ocnmsk=testdata(:,:)>0.
if (.not.any(sermsk)) then
  dataout(:,:,0)=1.
  dataout(:,:,1:)=0.
  return
end if

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) SHARED(sibdim,lsdata,sermsk,rlld,class_num,mapwater,datain,dataout,mthrng,ocnmsk) PRIVATE(ilat,ilon,pxy,i,nsum,wsum)
do ilat=1,sibdim(2)
  if ( mod(ilat,50)==0.or.ilat==sibdim(2) ) write(6,*) "ilat ",ilat,"/",sibdim(2)
  do ilon=1,sibdim(1)
    if (lsdata(ilon,ilat)<0.5) then
      if (.not.sermsk(ilon,ilat)) then
        call findnear(pxy,ilon,ilat,sermsk,rlld,sibdim)
        do i=1,class_num
          if ( .not.mapwater(i) ) then
            dataout(ilon,ilat,i)=datain(pxy(1),pxy(2),i)
            dataout(ilon,ilat,(i-1)*mthrng+class_num+1:i*mthrng+class_num) &
                =datain(pxy(1),pxy(2),(i-1)*mthrng+class_num+1:i*mthrng+class_num)
          end if
        end do
      end if
      nsum=sum(dataout(ilon,ilat,1:class_num),.not.mapwater)
      do i=1,class_num
        if ( .not.mapwater(i) ) then
          dataout(ilon,ilat,i)=dataout(ilon,ilat,i)*(1.-lsdata(ilon,ilat))/nsum
        end if
      end do
    else
      do i=1,class_num
        if ( .not.mapwater(i) ) then
          dataout(ilon,ilat,i)=0.
          dataout(ilon,ilat,(i-1)*mthrng+class_num+1:i*mthrng+class_num)=0.
        end if
      end do
    end if
    if (lsdata(ilon,ilat)>=0.5) then
      if (.not.ocnmsk(ilon,ilat)) then
        call findnear(pxy,ilon,ilat,ocnmsk,rlld,sibdim)
        dataout(ilon,ilat,0)=datain(pxy(1),pxy(2),0)
        do i=1,class_num
          if ( mapwater(i) ) then
            dataout(ilon,ilat,i)=datain(pxy(1),pxy(2),i)
          end if
        end do
      end if
      wsum=dataout(ilon,ilat,0)+sum(dataout(ilon,ilat,1:class_num),mapwater)
      dataout(ilon,ilat,0)=dataout(ilon,ilat,0)*lsdata(ilon,ilat)/wsum
      do i=1,class_num
        if ( mapwater(i) ) then
          dataout(ilon,ilat,i)=dataout(ilon,ilat,i)*lsdata(ilon,ilat)/wsum
        end if
      end do
    else
      dataout(ilon,ilat,0)=0.
      do i=1,class_num
        if ( mapwater(i) ) then
          dataout(ilon,ilat,i)=0.
        end if
      end do
    end if
  end do
end do
!$OMP END PARALLEL DO

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! clean real data
!

subroutine cleanreal(dataout,num,lsdata,rlld,sibdim)

implicit none

integer, intent(in) :: num
integer, dimension(2), intent(in) :: sibdim
real, dimension(sibdim(1),sibdim(2),0:num), intent(inout) :: dataout
real, dimension(sibdim(1),sibdim(2)), intent(in) :: lsdata
real, dimension(sibdim(1),sibdim(2),2), intent(in) :: rlld
logical, dimension(sibdim(1),sibdim(2)) :: sermsk
integer ilon,ilat,pxy(2)
real nsum

sermsk=.true.
do ilon=0,num
  sermsk=sermsk.and.(dataout(:,:,ilon)>0.)
end do
if (.not.any(sermsk)) return

call fill_cc_a(dataout(:,:,:),sibdim(1),num+1,sermsk)

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine rewrites the land/sea mask in the topography
! data file.
!

Subroutine cleantopo(topounit,toponame,topoout,lsmskin,oceanin,sibdim,zerozs)

use netcdf_m

Implicit None

Integer, intent(in) :: topounit
Integer, dimension(2), intent(in) :: sibdim
integer, dimension(3) :: spos,npos
integer, dimension(3) :: dimid
Integer ilout,ierr,ia,ib,i
integer ncid,lnctopo,varid
Character(len=*), intent(in) :: toponame,topoout
Character*80 formout
Character*47 dc
Real, dimension(sibdim(1),sibdim(2)), intent(in) :: lsmskin,oceanin
Real, dimension(sibdim(1),sibdim(2)) :: topo,sd,lsmsk
Real, dimension(sibdim(1),sibdim(2)) :: tmax, tmin
real, dimension(sibdim(2)) :: dum
Real, dimension(1) :: ra,rb,rc,rd
logical, intent(in) :: zerozs
ilout=Min(sibdim(1),30) ! To be compatiable with terread

Write(6,*) "Adjust topography data for consistancy with land-sea mask"

ierr=nf_open(toponame,nf_nowrite,ncid)
if (ierr==0) then
  lnctopo=1
  spos=1
  npos(1)=sibdim(1)
  npos(2)=sibdim(2)
  npos(3)=1
  ierr=nf_get_att_real(ncid,nf_global,'lon0',ra(1))
  ierr=nf_get_att_real(ncid,nf_global,'lat0',rb(1))
  ierr=nf_get_att_real(ncid,nf_global,'schmidt',rc(1))
  ierr=nf_inq_varid(ncid,'zs',varid)
  ierr=nf_get_vara_real(ncid,varid,spos,npos,topo)
  ierr=nf_inq_varid(ncid,'lsm',varid)
  ierr=nf_get_vara_real(ncid,varid,spos,npos,lsmsk)
  ierr=nf_inq_varid(ncid,'tsd',varid)
  ierr=nf_get_vara_real(ncid,varid,spos,npos,sd)
  ierr=nf_inq_varid(ncid,'zmax',varid)
  ierr=nf_get_vara_real(ncid,varid,spos,npos,tmax)
  ierr=nf_inq_varid(ncid,'zmin',varid)
  ierr=nf_get_vara_real(ncid,varid,spos,npos,tmin)
  ierr=nf_close(ncid)
else
  lnctopo=0
  Open(topounit,FILE=toponame,FORM='formatted',STATUS='old',IOSTAT=ierr)
  Read(topounit,*,IOSTAT=ierr) ia,ib,ra,rb,rc,rd,dc
  Read(topounit,*,IOSTAT=ierr) topo ! Topography data
  Read(topounit,*,IOSTAT=ierr) lsmsk ! land/sea mask (to be replaced)
  Read(topounit,*,IOSTAT=ierr) sd ! Topography standard deviation
  Close(topounit)
end if

if ( ierr/=0 ) then
  write(6,*) "ERROR: Cannot read file ",trim(toponame)
  call finishbanner
  stop -1
end if

lsmsk = real(1-nint(lsmskin))
if ( zerozs ) then
  where ( nint(oceanin)==1 .and. nint(lsmskin)==1 )
    topo(:,:) = 0.
    sd(:,:)   = 0.
    tmax(:,:) = 0.
    tmin(:,:) = 0.
  end where
end if

if (lnctopo==1) then
  ierr=nf_create(topoout,nf_clobber,ncid)
  if (ierr/=0) then
    write(6,*) "ERROR creating output topography file ",ierr
    call finishbanner
    stop -1
  end if
  ierr=nf_def_dim(ncid,'longitude',sibdim(1),dimid(1))
  ierr=nf_def_dim(ncid,'latitude',sibdim(2),dimid(2))
  ierr=nf_def_dim(ncid,'time',nf_unlimited,dimid(3))
  ierr=nf_def_var(ncid,'longitude',nf_float,1,dimid(1),varid)
  ierr=nf_def_var(ncid,'latitude',nf_float,1,dimid(2),varid)
  ierr=nf_def_var(ncid,'time',nf_float,1,dimid(3),varid)
  ierr=nf_def_var(ncid,'zs',nf_float,3,dimid(1:3),varid)
  ierr=nf_def_var(ncid,'lsm',nf_float,3,dimid(1:3),varid)
  ierr=nf_def_var(ncid,'tsd',nf_float,3,dimid(1:3),varid)
  ierr=nf_def_var(ncid,'zmax',nf_float,3,dimid(1:3),varid)
  ierr=nf_def_var(ncid,'zmin',nf_float,3,dimid(1:3),varid)
  ierr=nf_put_att_real(ncid,nf_global,'lon0',nf_real,1,ra)
  ierr=nf_put_att_real(ncid,nf_global,'lat0',nf_real,1,rb)
  ierr=nf_put_att_real(ncid,nf_global,'schmidt',nf_real,1,rc)
  ierr=nf_enddef(ncid)
  do i=1,sibdim(2)
    dum(i)=real(i)
  end do
  ierr=nf_inq_varid(ncid,'longitude',varid)
  ierr=nf_put_vara_real(ncid,varid,spos(1:1),npos(1:1),dum(1:sibdim(1)))
  ierr=nf_inq_varid(ncid,'latitude',varid)
  ierr=nf_put_vara_real(ncid,varid,spos(2:2),npos(2:2),dum(1:sibdim(2)))
  ierr=nf_inq_varid(ncid,'time',varid)
  dum(1)=0.
  ierr=nf_put_vara_real(ncid,varid,spos(3:3),npos(3:3),dum(1:1))
  ierr=nf_inq_varid(ncid,'zs',varid)
  ierr=nf_put_vara_real(ncid,varid,spos,npos,topo)
  ierr=nf_inq_varid(ncid,'lsm',varid)
  ierr=nf_put_vara_real(ncid,varid,spos,npos,lsmsk)
  ierr=nf_inq_varid(ncid,'tsd',varid)
  ierr=nf_put_vara_real(ncid,varid,spos,npos,sd)
  ierr=nf_inq_varid(ncid,'zmax',varid)
  ierr=nf_put_vara_real(ncid,varid,spos,npos,tmax)
  ierr=nf_inq_varid(ncid,'zmin',varid)
  ierr=nf_put_vara_real(ncid,varid,spos,npos,tmin)  
  ierr=nf_close(ncid)
else
  Open(topounit,FILE=topoout,FORM='formatted',STATUS='replace',IOSTAT=ierr)
  Write(topounit,'(i4,i6,2f10.3,f6.3,f10.0," ",a39)',IOSTAT=ierr) ia,ib,ra,rb,rc,rd,dc
  Write(formout,'("(",i3,"f7.0)")',IOSTAT=ierr) ilout
  Write(topounit,formout,IOSTAT=ierr) topo ! Topography data
  Write(formout,'("(",i3,"f4.1)")',IOSTAT=ierr) ilout
  Write(topounit,formout,IOSTAT=ierr) lsmsk ! land/sea mask
  Write(formout,'("(",i3,"f6.0)")',IOSTAT=ierr) ilout
  Write(topounit,formout,IOSTAT=ierr) sd ! Topography standard deviation
  Close(topounit)
end if

If (ierr.NE.0) then
  Write(6,*) "ERROR: Cannot write file ",trim(topoout)
  call finishbanner
  Stop -1
End if

Return
End

subroutine convertigbp(vtype,vfrac,vlai,sibdim,lsdata,rlld,class_num,mapindex,mapfrac,pft_len)

implicit none

integer, intent(in) :: class_num, pft_len
Integer, dimension(2), intent(in) :: sibdim
integer, dimension(sibdim(1),sibdim(2),9), intent(inout) :: vtype
integer, dimension(class_num,5), intent(in) :: mapindex
integer, dimension(1) :: pos
integer i, j, n, ipos, iv
integer iv_new, k
real, dimension(sibdim(1),sibdim(2),9), intent(inout) :: vfrac, vlai
real, dimension(class_num,5), intent(in) :: mapfrac
real, dimension(pft_len) :: newlai
real, dimension(pft_len) :: newgrid
real, dimension(sibdim(1),sibdim(2)), intent(in) :: lsdata
real, dimension(sibdim(1),sibdim(2),2), intent(in) :: rlld
real fc3, fc4, ftu, fg3, fg4, clat, nsum
real fmixed, fneedlebroad
real xp
real, parameter :: minfrac = 0.01        ! minimum non-zero tile fraction (improves load balancing)
Real, parameter :: pi = 3.1415926536
logical, dimension(pft_len) :: sermask

if ( any(mapindex>pft_len) ) then
  write(6,*) "ERROR: Unspecified index in mapconfig is not represented in pftconfig"
  call finishbanner
  stop -1
end if

if ( any(mapindex<0) .and. pft_len<18 ) then
  write(6,*) "ERROR: mapconfig contains special cases that require at least 18 PFTs to be defined"
  call finishbanner
  stop -1
end if

do i=1,class_num
  if ( abs(sum(mapfrac(i,:))-1.)>0.01 ) then
    write(6,*) "ERROR: mapconfig fractions do not sum to 1."
    write(6,*) "iveg,mapfactor ",i,mapfrac(i,:)
    call finishbanner
    stop -1
  end if
end do

write(6,*) "Mapping IGBP classes to CABLE PFTs"
!savannafrac(:,:) = 0.
do j = 1,sibdim(2)
  do i = 1,sibdim(1)
    if ( lsdata(i,j)<0.5 ) then
      newgrid     = 0.
      newlai      = 0.
          
      clat = rlld(i,j,2)
      ! grass
      if (abs(clat)>50.5) then
        fg3=0.
        fg4=0.
      else if (abs(clat)>49.5) then
        xp=abs(clat)-49.5
        fg3=(1.-xp)*0.9
        fg4=(1.-xp)*0.1
      else if (abs(clat)>40.5) then
        fg3=0.9
        fg4=0.1
      else if (abs(clat)>39.5) then
        xp=abs(clat)-39.5
        fg3=(1.-xp)*0.8+xp*0.9
        fg4=(1.-xp)*0.2+xp*0.1
      else if (abs(clat)>30.5) then
        fg3=0.8
        fg4=0.2
      else if (abs(clat)>29.5) then
        xp=abs(clat)-29.5
        fg3=(1.-xp)*0.5+xp*0.8
        fg4=(1.-xp)*0.5+xp*0.2
      else if (abs(clat)>25.5) then
        fg3=0.5
        fg4=0.5
      else if (abs(clat)>24.5) then
        xp=abs(clat)-24.5
        fg3=(1.-xp)*0.05+xp*0.5
        fg4=(1.-xp)*0.95+xp*0.5
      else
        fg3=0.05
        fg4=0.95
      end if
      ftu=max(1.-fg3-fg4,0.)
      ! crops
      if (abs(clat)>40.5) then
        fc3=1.
      else if (abs(clat)>39.5) then
        xp=abs(clat)-39.5
        fc3=(1.-xp)*0.9+xp
      else if (abs(clat)>30.5) then
        fc3=0.9
      else if (abs(clat)>29.5) then
        xp=abs(clat)-29.5
        fc3=(1.-xp)*0.7+xp*0.9
      else
        fc3=0.7
      end if
      fc4=max(1.-fc3,0.)
      ! mixed
      if (abs(clat)>25.5) then
        fmixed=0.5
      else if ( abs(clat)>24.5 ) then
        xp=abs(clat)-24.5
        fmixed=(1.-xp)*1.+xp*0.5
      else
        fmixed=1.
      end if
      ! needle/broad
      if (abs(clat)>40.5) then
        fneedlebroad=1.
      else if (abs(clat)>39.5) then
        xp=abs(clat)-39.5
        fneedlebroad=xp
      else
        fneedlebroad=0.
      endif
      
      do n = 1,9
        iv = vtype(i,j,n)
        do k=1,5
          iv_new=mapindex(iv,k)
          if ( iv_new>0 ) then
            newgrid(iv_new)=newgrid(iv_new)+vfrac(i,j,n)*mapfrac(iv,k)
            newlai(iv_new)=newlai(iv_new)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)
          else if ( iv_new==-1 ) then ! mixed
            newgrid(1)=newgrid(1)+vfrac(i,j,n)*mapfrac(iv,k)*(1.-fmixed)
            newlai(1)=newlai(1)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*(1.-fmixed)
            newgrid(4)=newgrid(4)+vfrac(i,j,n)*mapfrac(iv,k)*fmixed
            newlai(4)=newlai(4)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*fmixed
          else if ( iv_new==-2 ) then ! grass
            newgrid(6)=newgrid(6)+vfrac(i,j,n)*mapfrac(iv,k)*fg3
            newlai(6)=newlai(6)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*fg3
            newgrid(7)=newgrid(7)+vfrac(i,j,n)*mapfrac(iv,k)*fg4
            newlai(7)=newlai(7)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*fg4
            newgrid(8)=newgrid(8)+vfrac(i,j,n)*mapfrac(iv,k)*ftu
            newlai(8)=newlai(8)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*ftu
          else if ( iv_new==-3 ) then ! needle/broad (non-savanna)
            newgrid(1)=newgrid(1)+vfrac(i,j,n)*mapfrac(iv,k)*fneedlebroad
            newlai(1)=newlai(1)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*fneedlebroad
            newgrid(2)=newgrid(2)+vfrac(i,j,n)*mapfrac(iv,k)*(1.-fneedlebroad)
            newlai(2)=newlai(2)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*(1.-fneedlebroad)
          else if ( iv_new==-4 ) then ! crop
            newgrid(9)=newgrid(9)+vfrac(i,j,n)*mapfrac(iv,k)*fc3
            newlai(9)=newlai(9)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*fc3
            newgrid(10)=newgrid(10)+vfrac(i,j,n)*mapfrac(iv,k)*fc4
            newlai(10)=newlai(10)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*fc4
          else if ( iv_new==-5 ) then ! needle/broad (savanna)
            newgrid(1)=newgrid(1)+vfrac(i,j,n)*mapfrac(iv,k)*fneedlebroad
            newlai(1)=newlai(1)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*fneedlebroad
            newgrid(18)=newgrid(18)+vfrac(i,j,n)*mapfrac(iv,k)*(1.-fneedlebroad)
            newlai(18)=newlai(18)+vfrac(i,j,n)*vlai(i,j,n)*mapfrac(iv,k)*(1.-fneedlebroad)
          else if ( iv_new<-100 .and.iv_new>-109 ) then
            ! urban
          else if ( iv_new/=0 ) then
            write(6,*) "ERROR: Unknown index ",iv_new
            call finishbanner
            stop -1
          end if
        end do
      end do
      where ( newgrid(:)>0. )
        newlai(:) = newlai(:)/newgrid(:)
      end where
      ipos = count(newgrid(:)>0.)
      sermask = .true.
      sermask(6:7) = .false.
      sermask(9:10) = .false.
      do while ( ipos>9 )
        pos = minloc(newgrid(:), newgrid(:)>0. .and. sermask)
        newgrid(pos(1)) = 0.
        nsum = sum(newgrid(:))
        newgrid(:) = newgrid(:)/nsum
        ipos = count(newgrid(:)>0.)
      end do    
      !do while ( any(newgrid(:)<minfrac.and.newgrid(:)>0..and.sermask) )
      !  pos = minloc(newgrid(:), newgrid(:)>0.)
      !  newgrid(pos(1)) = 0.
      !  nsum = sum(newgrid(:))
      !  newgrid(:) = newgrid(:)/nsum
      !end do

      n = 0
      vtype(i,j,:) = 0
      vfrac(i,j,:) = 0.
      vlai(i,j,:)  = 0.
      do iv = 1,pft_len
        if ( newgrid(iv)>0. ) then
          n = n + 1
          vtype(i,j,n) = iv
          vfrac(i,j,n) = newgrid(iv)
          vlai(i,j,n)  = newlai(iv)
        end if
      end do
      
    end if
  end do
end do

return
end subroutine convertigbp
    
subroutine findentry(largestring,iposbeg,iposend,failok)

implicit none

character(len=*), intent(in) :: largestring
integer, intent(inout) :: iposbeg, iposend
integer test
logical, intent(in) :: failok

test=verify(largestring(iposend+1:),' ')
if ( test==0 ) then
  iposbeg=-1
  iposend=-1
  if (failok) return
  write(6,*) "ERROR: Cannot find entry in ",trim(largestring)
  call finishbanner
  stop -1
end if
iposbeg=test+iposend
test=scan(largestring(iposbeg:),' ')
if ( test==0 ) then
  iposbeg=-1
  iposend=-1
  if (failok) return
  write(6,*) "ERROR: Cannot find entry in ",trim(largestring)
  call finishbanner
  stop -1
end if
iposend=test+iposbeg-2

return
end subroutine findentry

subroutine findentry_real(largestring,iposbeg,iposend,failok,rfrac)

implicit none

character(len=*), intent(in) :: largestring
integer, intent(inout) :: iposbeg, iposend
real, intent(out) :: rfrac
integer ioerror
logical, intent(in) :: failok

call findentry(largestring,iposbeg,iposend,failok)
if ( iposbeg==-1 .and. failok ) return
read(largestring(iposbeg:iposend),*,iostat=ioerror) rfrac
if ( ioerror/=0 ) then
  write(6,*) "ERROR: Cannot read mapconfig line"
  write(6,*) trim(largestring)
  call finishbanner
  stop -1
end if

return
end subroutine findentry_real
    
subroutine findentry_integer(largestring,iposbeg,iposend,failok,jveg)

implicit none

character(len=*), intent(in) :: largestring
integer, intent(inout) :: iposbeg, iposend
integer, intent(out) :: jveg
integer ioerror
logical, intent(in) :: failok

call findentry(largestring,iposbeg,iposend,failok)
if ( iposbeg==-1 .and. failok ) return
read(largestring(iposbeg:iposend),*,iostat=ioerror) jveg
if ( ioerror/=0 ) then
  write(6,*) "ERROR: Cannot read mapconfig line"
  write(6,*) trim(largestring)
  call finishbanner
  stop -1
end if

return
end subroutine findentry_integer

subroutine findentry_character(largestring,iposbeg,iposend,failok,jdesc)

implicit none

character(len=*), intent(in) :: largestring
integer, intent(inout) :: iposbeg, iposend
character(len=*), intent(out) :: jdesc
logical, intent(in) :: failok

call findentry(largestring,iposbeg,iposend,failok)
if ( iposbeg==-1 .and. failok ) return
jdesc=largestring(iposbeg:iposend)

return
end subroutine findentry_character

subroutine findentry_logical(largestring,iposbeg,iposend,failok,maplogical)

implicit none

character(len=*), intent(in) :: largestring
integer, intent(inout) :: iposbeg, iposend
logical, intent(out) :: maplogical
integer ioerror
logical, intent(in) :: failok

call findentry(largestring,iposbeg,iposend,failok)
if ( iposbeg==-1 .and. failok ) return
read(largestring(iposbeg:iposend),*,iostat=ioerror) maplogical
if ( ioerror/=0 ) then
  write(6,*) "ERROR: Cannot read mapconfig line"
  write(6,*) trim(largestring)
  call finishbanner
  stop -1
end if

return
end subroutine findentry_logical
    
subroutine findindex(kdesc,pft_desc,pft_len,ateb_desc,ateb_len,mapindex)

implicit none

integer, intent(in) :: pft_len, ateb_len
integer, intent(out) :: mapindex
integer k
character(len=*), intent(in) :: kdesc
character(len=*), dimension(pft_len), intent(in) :: pft_desc
character(len=*), dimension(ateb_len), intent(in) :: ateb_desc
logical matchfound

if ( trim(kdesc)=="(Mixed)" .or. trim(kdesc)=="(mixed)" ) then    
  mapindex = -1  
else if ( trim(kdesc)=="(Grass)" .or. trim(kdesc)=="(grass)" ) then
  mapindex = -2  
else if ( trim(kdesc)=="(ENB)" ) then    
  mapindex = -3  
else if ( trim(kdesc)=="(Crop)" .or. trim(kdesc)=="(crop)" ) then
  mapindex = -4
else if ( trim(kdesc)=="(ENB_Savanna)" .or. trim(kdesc)=="(ENB_savanna)" ) then
  mapindex = -5
else if ( trim(kdesc)=="(Urban-generic)" .or. trim(kdesc)=="(urban-generic)" .or. trim(kdesc)=="(Urban-Generic)" ) then
  mapindex = -101  
else if ( trim(kdesc)=="(Urban-low)" .or. trim(kdesc)=="(urban-low)" .or. trim(kdesc)=="(Urban-Low)" ) then
  mapindex = -102  
else if ( trim(kdesc)=="(Urban-medium)" .or. trim(kdesc)=="(urban-medium)" .or. trim(kdesc)=="(Urban-Medium)" ) then
  mapindex = -103  
else if ( trim(kdesc)=="(Urban-high)" .or. trim(kdesc)=="(urban-high)" .or. trim(kdesc)=="(Urban-High)" ) then
  mapindex = -104  
else if ( trim(kdesc)=="(Urban-cbd)" .or. trim(kdesc)=="(urban-cbd)" .or. trim(kdesc)=="(Urban-CBD)" ) then
  mapindex = -105  
else if ( trim(kdesc)=="(Industrial-low)" .or. trim(kdesc)=="(industrial-low)" .or. trim(kdesc)=="(Industrial-Low)" ) then
  mapindex = -106  
else if ( trim(kdesc)=="(Industrial-medium)" .or. trim(kdesc)=="(industrial-medium)" .or. trim(kdesc)=="(Industrial-Medium)" ) then
  mapindex = -107  
else if ( trim(kdesc)=="(Industrial-high)" .or. trim(kdesc)=="(industrial-high)" .or. trim(kdesc)=="(Industrial-High)" ) then
  mapindex = -108  
else    
  matchfound=.false.
  do k=1,pft_len
    if ( trim(kdesc)==trim(pft_desc(k)) ) then
      matchfound=.true.
      mapindex=k
      exit  
    end if
  end do
  if ( .not.matchfound ) then
    do k = 1,ateb_len
      if ( trim(kdesc)==trim(ateb_desc(k)) ) then
        matchfound = .true.
        mapindex = -100 - k
      end if
    end do    
  end if    
  if ( .not.matchfound ) then
    write(6,*) "ERROR: Cannot find ",trim(kdesc)," in PFT or urban list"
    call finishbanner
    stop -1
  end if
end if

return
end subroutine findindex
