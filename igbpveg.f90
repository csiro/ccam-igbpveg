! Conformal Cubic Atmospheric Model
    
! Copyright 2015 Commonwealth Scientific Industrial Research Organisation (CSIRO)
    
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

character*80, dimension(:,:), allocatable :: options
character*130, dimension(8) :: fname
character*130 topofile
character*130 landtypeout
character*130 newtopofile
character*130 outputmode
character*130 veginput, soilinput, laiinput, albvisinput, albnirinput
integer binlimit, nopts, month
integer outmode
logical fastigbp,igbplsmask,ozlaipatch,tile

namelist/vegnml/ topofile,fastigbp,                  &
                 landtypeout,igbplsmask,newtopofile, &
                 binlimit,month,ozlaipatch,          &
                 tile,outputmode, veginput,          &
                 soilinput, laiinput, albvisinput,   &
                 albnirinput

! Start banner
write(6,*) "=================================================================================="
write(6,*) "CCAM: Starting igbpveg"
write(6,*) "=================================================================================="

write(6,*) 'IGBPVEG - IGBP 1km to CC grid (FEB-16)'

! Read switches
nopts=1
allocate (options(nopts,2))
options(:,1) = (/ '-s' /)
options(:,2) = ''

call readswitch(options,nopts)
call defaults(options,nopts)

outputmode=''
ozlaipatch=.false.

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

outmode=0
if ( outputmode=='cablepft' ) then
  outmode=1
end if

call createveg(options,nopts,fname,fastigbp,igbplsmask,ozlaipatch,tile,month,binlimit,outmode)

deallocate(options)

! Complete
write(6,*) "CCAM: igbpveg completed successfully"
      
! End banner
write(6,*) "=================================================================================="
write(6,*) "CCAM: Finished igbpveg"
write(6,*) "=================================================================================="

stop
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
Write(6,*) '    topofile="topout"'
Write(6,*) '    newtopofile="newtopout"'
Write(6,*) '    landtypeout="veg"'
Write(6,*) '    veginput="gigbp2_0ll.img"'
Write(6,*) '    soilinput="usda4.img"'
Write(6,*) '    laiinput="slai01.img"'
Write(6,*) '    albvisinput="albvis223.img"'
Write(6,*) '    albnirinput="albnir223.img"'
Write(6,*) '    fastigbp=t'
Write(6,*) '    igbplsmask=t'
!Write(6,*) '    ozlaipatch=f'
Write(6,*) '    tile=t'
Write(6,*) '    binlimit=2'
Write(6,*) '    outputmode="cablepft"'
Write(6,*) '  &end'
Write(6,*)
Write(6,*) '  where:'
Write(6,*) '    month         = the month to process (1-12, 0=all)'
Write(6,*) '    topofile      = topography (input) file'
Write(6,*) '    newtopofile   = Output topography file name'
Write(6,*) '                    (if igbplsmask=t)'
Write(6,*) '    landtypeout   = Land-use filename'
Write(6,*) '    veginput      = Location of IGBP input file'
Write(6,*) '    soilinput     = Location of USDA input file'
Write(6,*) '    laiinput      = Location of LAI input file for month>0'
Write(6,*) '                    or path to LAI files for month=0'
Write(6,*) '    albvisinput   = Location of VIS Albedo input file'
Write(6,*) '    albnirinput   = Location of NIR Albedo input file'
Write(6,*) '    fastigbp      = Turn on fastigbp mode (see notes below)'
Write(6,*) '    igbplsmask    = Define land/sea mask from IGBP dataset'
!Write(6,*) '    ozlaipath     = Use CSIRO LAI dataset for Australia'
Write(6,*) '    tile          = Seperate land cover into tiles'
Write(6,*) '    binlimit      = The minimum ratio between the grid'
Write(6,*) '                    length scale and the length scale of'
Write(6,*) '                    the aggregated land-use data (see notes'
Write(6,*) '                    below).'
Write(6,*) '    outputmode    = format of output file.'
Write(6,*) '                    igbp     Use IGBP classes (default)'
Write(6,*) '                    cablepft Use CABLE PFTs'
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
Write(6,*)
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

Subroutine createveg(options,nopts,fname,fastigbp,igbplsmask,ozlaipatch,tile,month,binlimit,outmode)

Use ccinterp

Implicit None

Logical, intent(in) :: fastigbp,igbplsmask,ozlaipatch,tile
Integer, intent(in) :: nopts,binlimit,month,outmode
Character(len=*), dimension(nopts,2), intent(in) :: options
Character(len=*), dimension(8), intent(in) :: fname
character*90 filename
Character*80, dimension(1:3) :: outputdesc
Character*80 returnoption,csize,filedesc
Character*47 header
Character*9 formout
Character*2 monthout
real, dimension(:,:,:), allocatable :: vlai
Real, dimension(:,:,:), allocatable :: landdata,soildata,rlld,vfrac,tmp
Real, dimension(:,:), allocatable :: gridout,lsdata,urbandata,oceandata,albvisdata,albnirdata
real, dimension(:,:), allocatable :: savannafrac
real, dimension(:,:), allocatable :: rdata
Real, dimension(3,2) :: alonlat
Real, dimension(2) :: lonlat
Real, dimension(1) :: atime
Real, dimension(1) :: alvl
Real schmidt,dsx,ds,urbanfrac
integer, dimension(:,:), allocatable :: idata
integer, dimension(:,:,:), allocatable :: vtype
Integer, dimension(2) :: sibdim
Integer, dimension(4) :: dimnum,dimid,dimcount
Integer, dimension(0:4) :: ncidarr
Integer, dimension(6) :: adate
Integer, dimension(2:21) :: varid
Integer sibsize,tunit,i,j,k,ierr,sibmax(1),mthrng
integer tt
logical, dimension(16) :: sermsk

mthrng=1
if ( month==0 ) then
  mthrng=12
end if
if ( month<0 .or. month>12 ) then
  write(6,*) "ERROR: Invalid month ",month
  write(6,*) "Must be between 0 and 12"
  stop
end if

csize=returnoption('-s',options,nopts)
read(csize,FMT=*,IOSTAT=ierr) sibsize
if (ierr/=0) then
  write(6,*) 'ERROR: Invalid array size.  Must be an integer.'
  stop
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

allocate(albvisdata(sibdim(1),sibdim(2)))
allocate(albnirdata(sibdim(1),sibdim(2)))
allocate(soildata(sibdim(1),sibdim(2),0:8))
allocate(landdata(sibdim(1),sibdim(2),0:17+16*mthrng))

! Read igbp data
call getdata(landdata,lonlat,gridout,rlld,sibdim,17+16*mthrng,sibsize,'land',fastigbp,ozlaipatch,binlimit,month,fname(4),fname(6))
call getdata(soildata,lonlat,gridout,rlld,sibdim,8,sibsize,'soil',fastigbp,ozlaipatch,binlimit,month,fname(5),fname(6))
call getdata(albvisdata,lonlat,gridout,rlld,sibdim,0,sibsize,'albvis',fastigbp,ozlaipatch,binlimit,month,fname(7),fname(6))
call getdata(albnirdata,lonlat,gridout,rlld,sibdim,0,sibsize,'albnir',fastigbp,ozlaipatch,binlimit,month,fname(8),fname(6))

deallocate(gridout)
allocate(urbandata(sibdim(1),sibdim(2)),lsdata(sibdim(1),sibdim(2)),oceandata(sibdim(1),sibdim(2)))

write(6,*) "Preparing data..."
! extract urban cover and remove from landdata
urbandata(:,:)=landdata(:,:,13)
call igbpfix(landdata,rlld,sibdim,mthrng)

if (igbplsmask) then
  write(6,*) "Using IGBP land/sea mask"
  where ((landdata(:,:,0)+landdata(:,:,17))>0.)
    oceandata=landdata(:,:,0)/(landdata(:,:,0)+landdata(:,:,17))
  elsewhere
    oceandata=0.
  end where
  lsdata=real(nint(landdata(:,:,0)+landdata(:,:,17)))
  call cleantopo(tunit,fname(1),fname(3),lsdata,oceandata,sibdim)
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
call cleanigbp(landdata,lsdata,rlld,sibdim,mthrng)
write(6,*) "Clean soil data"
call cleanreal(soildata,8,lsdata,rlld,sibdim)
write(6,*) "Calculate soil texture"
call calsoilnear(landdata,soildata,lsdata,sibdim,idata)
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
allocate( vfrac(sibdim(1),sibdim(2),5), vtype(sibdim(1),sibdim(2),5) )
allocate( vlai(sibdim(1),sibdim(2),5) )
allocate( savannafrac(sibdim(1),sibdim(2)) )

write(6,*) "Create output file"
dimnum(1:2)=sibdim(1:2) ! CC grid dimensions
dimnum(3)=1 ! Turn off level
dimnum(4)=1 ! Number of months in a year
adate=0 ! Turn off date
adate(2)=1 ! time units=months

! Prep nc output
do tt=1,mthrng
  write(6,*) "Writing month ",tt,"/",mthrng

  if (mthrng==1) then
    filename=fname(2)
  else
    write(filename,"(A,'.',I2.2)") trim(fname(2)),tt
  end if

  call ncinitcc(ncidarr,filename,dimnum(1:3),dimid,adate)
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
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(12),1.,0.)

  outputdesc(1)='lai2'
  outputdesc(2)='Leaf Area Index (tile2)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(17),1.,0.)
  outputdesc(1)='vegt2'
  outputdesc(2)='Land-use classification (tile2)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(8),1.,0.)
  outputdesc(1)='vfrac2'
  outputdesc(2)='Land-use cover fraction (tile2)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(13),1.,0.)

  outputdesc(1)='lai3'
  outputdesc(2)='Leaf Area Index (tile3)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(18),1.,0.)
  outputdesc(1)='vegt3'
  outputdesc(2)='Land-use classification (tile3)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(9),1.,0.)
  outputdesc(1)='vfrac3'
  outputdesc(2)='Land-use cover fraction (tile3)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(14),1.,0.)

  outputdesc(1)='lai4'
  outputdesc(2)='Leaf Area Index (tile4)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(19),1.,0.)
  outputdesc(1)='vegt4'
  outputdesc(2)='Land-use classification (tile4)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(10),1.,0.)
  outputdesc(1)='vfrac4'
  outputdesc(2)='Land-use cover fraction (tile4)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(15),1.,0.)

  outputdesc(1)='lai5'
  outputdesc(2)='Leaf Area Index (tile5)'
  outputdesc(3)=''
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(20),1.,0.)
  outputdesc(1)='vegt5'
  outputdesc(2)='Land-use classification (tile5)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(11),1.,0.)
  outputdesc(1)='vfrac5'
  outputdesc(2)='Land-use cover fraction (tile5)'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(16),1.,0.)

  outputdesc(1)='urban'
  outputdesc(2)='Urban fraction'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(7),1.,0.)

  outputdesc(1)='savanna'
  outputdesc(2)='Savanna fraction of PFT=2'
  outputdesc(3)='none'
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(21),1.,0.)
  
  call ncatt(ncidarr,'lon0',lonlat(1))
  call ncatt(ncidarr,'lat0',lonlat(2))
  call ncatt(ncidarr,'schmidt',schmidt)
  call ncatt(ncidarr,'cableversion',223.) ! CABLE version for data
  if ( outmode==1 ) then
    call ncatt(ncidarr,'cableformat',1.)    
  else
    call ncatt(ncidarr,'cableformat',0.)
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
  do j=1,sibdim(2)
    do i=1,sibdim(1)
      if (lsdata(i,j)>=0.5) then
        vtype(i,j,1:5)=0
        vfrac(i,j,1)=1.
        vfrac(i,j,2:5)=0.
        vlai(i,j,:)=0.
      else
        sermsk=.true.
        do k=1,5
          sibmax=maxloc(landdata(i,j,1:16),sermsk)
          sermsk(sibmax(1))=.false.
          vtype(i,j,k)=sibmax(1)
          vfrac(i,j,k)=landdata(i,j,sibmax(1))
          vlai(i,j,k)=landdata(i,j,17+(sibmax(1)-1)*mthrng+tt)
        end do
        if (.not.tile) then
          vlai(i,j,1)=sum(vlai(i,j,:)*vfrac(i,j,:))/sum(vfrac(i,j,:))
          vlai(i,j,2:5)=0.
          vfrac(i,j,1)=1.
          vfrac(i,j,2:5)=0.        
        end if
        vfrac(i,j,:)=vfrac(i,j,:)/sum(vfrac(i,j,:))
        vfrac(i,j,1:4)=real(nint(100.*vfrac(i,j,1:4)))/100.
        vfrac(i,j,5)=1.-sum(vfrac(i,j,1:4))
        do k=5,2,-1
          if ((vfrac(i,j,k)<0.).or.(vfrac(i,j,k-1)<vfrac(i,j,k))) then
            vfrac(i,j,k-1)=vfrac(i,j,k-1)+vfrac(i,j,k)
            vfrac(i,j,k)=0.
          end if
        end do
      end if
    end do
  end do
  if ( outmode==1 ) then
    call convertigbp(vtype,vfrac,vlai,savannafrac,sibdim,lsdata,rlld)
  end if
  dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
  rdata=Real(vtype(:,:,1))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(6))
  rdata=Real(vtype(:,:,2))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(8))
  rdata=Real(vtype(:,:,3))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(9))
  rdata=Real(vtype(:,:,4))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(10))
  rdata=Real(vtype(:,:,5))
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(11))
  call ncwritedatgen(ncidarr,vfrac(:,:,1),dimcount,varid(12))
  call ncwritedatgen(ncidarr,vfrac(:,:,2),dimcount,varid(13))
  call ncwritedatgen(ncidarr,vfrac(:,:,3),dimcount,varid(14))
  call ncwritedatgen(ncidarr,vfrac(:,:,4),dimcount,varid(15))
  call ncwritedatgen(ncidarr,vfrac(:,:,5),dimcount,varid(16))
  call ncwritedatgen(ncidarr,vlai(:,:,1),dimcount,varid(5))
  call ncwritedatgen(ncidarr,vlai(:,:,2),dimcount,varid(17))
  call ncwritedatgen(ncidarr,vlai(:,:,3),dimcount,varid(18))
  call ncwritedatgen(ncidarr,vlai(:,:,4),dimcount,varid(19))
  call ncwritedatgen(ncidarr,vlai(:,:,5),dimcount,varid(20))

  ! Urban
  write(6,*) 'Write urban fraction'
  dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
  urbanfrac=1.
  rdata=urbandata*urbanfrac
  call ncwritedatgen(ncidarr,rdata,dimcount,varid(7))

  ! Savanna fraction 
  if ( outmode==1 ) then
    dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
    call ncwritedatgen(ncidarr,savannafrac,dimcount,varid(21))
  end if
  
  call ncclose(ncidarr)

end do

deallocate(landdata,urbandata,lsdata)
deallocate(vfrac,vtype,idata,vlai)
deallocate(savannafrac)
deallocate(rlld)
deallocate(rdata)

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Fix IGBP data
!

subroutine igbpfix(landdata,rlld,sibdim,mthrng)

implicit none

integer, intent(in) :: mthrng
integer, dimension(1:2), intent(in) :: sibdim
real, dimension(sibdim(1),sibdim(2),1:2), intent(in) :: rlld
real, dimension(sibdim(1),sibdim(2),0:17+16*mthrng), intent(inout) :: landdata
real, dimension(sibdim(1),sibdim(2),1:16+16*mthrng) :: newdata
logical, dimension(1:sibdim(1),1:sibdim(2)) :: allmsk
integer i,ilon,ilat,newsize
real nsum,wsum

landdata(:,:,13)=0. ! remove urban

allmsk=sum(landdata(:,:,1:16),3)>0.
if (.not.any(allmsk)) return

newdata(:,:,1:16)=landdata(:,:,1:16)
newdata(:,:,17:)=landdata(:,:,18:)
newsize=16*(mthrng+1)
!call fill_cc_a(newdata,sibdim(1),newsize,allmsk)
do i=1,newsize
  write(6,*) "Fill class ",i
  call fill_cc(newdata(:,:,i),sibdim(1),allmsk)
end do

do ilat=1,sibdim(2)
  do ilon=1,sibdim(1)
    wsum=landdata(ilon,ilat,0)+landdata(ilon,ilat,17) ! water
    if (wsum<1.) then
      if (.not.allmsk(ilon,ilat)) then
        landdata(ilon,ilat,1:16)=newdata(ilon,ilat,1:16)  
        landdata(ilon,ilat,18:17+16*mthrng)=newdata(ilon,ilat,17:16+16*mthrng)
      end if
      nsum=sum(landdata(ilon,ilat,1:16)) ! land      
      landdata(ilon,ilat,1:16)=landdata(ilon,ilat,1:16)*max(1.-wsum,0.)/nsum
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

subroutine cleanigbp(dataout,lsdata,rlld,sibdim,mthrng)

implicit none

integer, intent(in) :: mthrng
integer, dimension(2), intent(in) :: sibdim
real, dimension(sibdim(1),sibdim(2),0:17+16*mthrng), intent(inout) :: dataout
real, dimension(sibdim(1),sibdim(2)), intent(in) :: lsdata
real, dimension(sibdim(1),sibdim(2),2), intent(in) :: rlld
real, dimension(sibdim(1),sibdim(2),0:17+16*mthrng) :: datain
logical, dimension(sibdim(1),sibdim(2)) :: sermsk,ocnmsk
integer ilon,ilat,pxy(2)
real nsum,wsum

datain=dataout
sermsk=sum(datain(:,:,1:16),3)>0.
ocnmsk=(datain(:,:,0)+datain(:,:,17))>0.
if (.not.any(sermsk)) then
  dataout(:,:,0)=1.
  dataout(:,:,1:)=0.
  return
end if

do ilat=1,sibdim(2)
  do ilon=1,sibdim(1)
    if (lsdata(ilon,ilat)<0.5) then
      if (.not.sermsk(ilon,ilat)) then
        call findnear(pxy,ilon,ilat,sermsk,rlld,sibdim)
        dataout(ilon,ilat,1:16)=datain(pxy(1),pxy(2),1:16)
        dataout(ilon,ilat,18:)=datain(pxy(1),pxy(2),18:)
      end if
      nsum=sum(dataout(ilon,ilat,1:16))
      dataout(ilon,ilat,1:16)=dataout(ilon,ilat,1:16)*(1.-lsdata(ilon,ilat))/nsum
    else
      dataout(ilon,ilat,1:16)=0.
      dataout(ilon,ilat,18:)=0.
    end if
    if (lsdata(ilon,ilat)>=0.5) then
      if (.not.ocnmsk(ilon,ilat)) then
        call findnear(pxy,ilon,ilat,ocnmsk,rlld,sibdim)
        dataout(ilon,ilat,0)=datain(pxy(1),pxy(2),0)
        dataout(ilon,ilat,17)=datain(pxy(1),pxy(2),17)
      end if
      wsum=dataout(ilon,ilat,0)+dataout(ilon,ilat,17)
      dataout(ilon,ilat,0)=dataout(ilon,ilat,0)*lsdata(ilon,ilat)/wsum
      dataout(ilon,ilat,17)=dataout(ilon,ilat,17)*lsdata(ilon,ilat)/wsum
    else
      dataout(ilon,ilat,0)=0.
      dataout(ilon,ilat,17)=0.
    end if
  end do
end do

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

Subroutine cleantopo(topounit,toponame,topoout,lsmskin,oceanin,sibdim)

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
real, dimension(sibdim(2)) :: dum
Real, dimension(1) :: ra,rb,rc,rd
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
  stop
end if

lsmsk = real(1-nint(lsmskin))
where ( nint(oceanin)==1 .and. nint(lsmskin)==1 )
  topo(:,:) = 0.
  sd(:,:)   = 0.
end where

if (lnctopo==1) then
  ierr=nf_create(topoout,nf_clobber,ncid)
  if (ierr/=0) then
    write(6,*) "ERROR creating output topography file ",ierr
    stop
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
  Stop
End if

Return
End

subroutine convertigbp(vtype,vfrac,vlai,savannafrac,sibdim,lsdata,rlld)

implicit none

integer, parameter :: mxvt=17
Integer, dimension(2), intent(in) :: sibdim
integer, dimension(sibdim(1),sibdim(2),5), intent(inout) :: vtype
integer, dimension(1) :: pos
integer i, j, n, ipos, iv
real, dimension(sibdim(1),sibdim(2),5), intent(inout) :: vfrac, vlai
real, dimension(mxvt) :: newlai
real, dimension(mxvt) :: newgrid
real, dimension(sibdim(1),sibdim(2)), intent(out) :: savannafrac
real, dimension(sibdim(1),sibdim(2)), intent(in) :: lsdata
real, dimension(sibdim(1),sibdim(2),2), intent(in) :: rlld
real fc3, fc4, ftu, fg3, fg4, clat, nsum
real xp
real, parameter :: minfrac = 0.01        ! minimum non-zero tile fraction (improves load balancing)
Real, parameter :: pi = 3.1415926536

write(6,*) "Mapping IGBP classes to CABLE PFTs"
savannafrac(:,:) = 0.
do j = 1,sibdim(2)
  do i = 1,sibdim(1)
    if ( lsdata(i,j)<0.5 ) then
      newgrid     = 0.
      newlai      = 0.
          
      clat = rlld(i,j,2)*180./pi
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
      ftu=1.-fg3-fg4
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
      fc4=1.-fc3
      do n = 1,5
        iv = vtype(i,j,n)
        select case (iv)
          case (1,2,3,4,11)
            newgrid(iv)=newgrid(iv)+vfrac(i,j,n)
            newlai(iv)=newlai(iv)+vfrac(i,j,n)*vlai(i,j,n)
          case (5)
            if (abs(clat)>25.5) then
              newgrid(1)=newgrid(1)+vfrac(i,j,n)*0.5
              newlai(1)=newlai(1)+vfrac(i,j,n)*0.5*vlai(i,j,n)
              newgrid(4)=newgrid(4)+vfrac(i,j,n)*0.5
              newlai(4)=newlai(4)+vfrac(i,j,n)*0.5*vlai(i,j,n)
            else if (abs(clat)>24.5) then
              xp=abs(clat)-24.5
              newgrid(1)=newgrid(1)+vfrac(i,j,n)*0.5*xp
              newlai(1)=newlai(1)+vfrac(i,j,n)*0.5*vlai(i,j,n)*xp
              newgrid(4)=newgrid(4)+vfrac(i,j,n)*(1.-0.5*xp)
              newlai(4)=newlai(4)+vfrac(i,j,n)*vlai(i,j,n)*(1.-0.5*xp)
            else
              newgrid(4)=newgrid(4)+vfrac(i,j,n)
              newlai(4)=newlai(4)+vfrac(i,j,n)*vlai(i,j,n)
            end if
          case (6)
            newgrid(5)=newgrid(5)+vfrac(i,j,n)*0.8
            newlai(5)=newlai(5)+vfrac(i,j,n)*0.8*vlai(i,j,n)
            newgrid(6)=newgrid(6)+vfrac(i,j,n)*0.2*fg3
            newlai(6)=newlai(6)+vfrac(i,j,n)*0.2*fg3*vlai(i,j,n)
            newgrid(7)=newgrid(7)+vfrac(i,j,n)*0.2*fg4
            newlai(7)=newlai(7)+vfrac(i,j,n)*0.2*fg4*vlai(i,j,n)
            newgrid(8)=newgrid(8)+vfrac(i,j,n)*0.2*ftu
            newlai(8)=newlai(8)+vfrac(i,j,n)*0.2*ftu*vlai(i,j,n)
          case (7)
            newgrid(5)=newgrid(5)+vfrac(i,j,n)*0.2
            newlai(5)=newlai(5)+vfrac(i,j,n)*0.2*vlai(i,j,n)
            newgrid(6)=newgrid(6)+vfrac(i,j,n)*0.8*fg3
            newlai(6)=newlai(6)+vfrac(i,j,n)*0.8*fg3*vlai(i,j,n)
            newgrid(7)=newgrid(7)+vfrac(i,j,n)*0.8*fg4
            newlai(7)=newlai(7)+vfrac(i,j,n)*0.8*fg4*vlai(i,j,n)
            newgrid(8)=newgrid(8)+vfrac(i,j,n)*0.8*ftu
            newlai(8)=newlai(8)+vfrac(i,j,n)*0.8*ftu*vlai(i,j,n)
          case (8)
            if (abs(clat)>40.5) then
              newgrid(1)=newgrid(1)+vfrac(i,j,n)*0.4
              newlai(1)=newlai(1)+vfrac(i,j,n)*0.4*vlai(i,j,n)
            else if (abs(clat)>39.5) then
              xp=abs(clat)-39.5
              newgrid(1)=newgrid(1)+vfrac(i,j,n)*0.4*xp
              newlai(1)=newlai(1)+vfrac(i,j,n)*vlai(i,j,n)*0.4*xp
              savannafrac(i,j)=savannafrac(i,j)+vfrac(i,j,n)*0.4*(1.-xp)
              newgrid(2)=newgrid(2)+vfrac(i,j,n)*0.4*(1.-xp)
              newlai(2)=newlai(2)+vfrac(i,j,n)*vlai(i,j,n)*0.4*(1.-xp)
            else
              savannafrac(i,j)=savannafrac(i,j)+vfrac(i,j,n)*0.4
              newgrid(2)=newgrid(2)+vfrac(i,j,n)*0.4
              newlai(2)=newlai(2)+vfrac(i,j,n)*0.4*vlai(i,j,n)
            end if
            newgrid(6)=newgrid(6)+vfrac(i,j,n)*0.6*fg3
            newlai(6)=newlai(6)+vfrac(i,j,n)*0.6*fg3*vlai(i,j,n)
            newgrid(7)=newgrid(7)+vfrac(i,j,n)*0.6*fg4
            newlai(7)=newlai(7)+vfrac(i,j,n)*0.6*fg4*vlai(i,j,n)
            newgrid(8)=newgrid(8)+vfrac(i,j,n)*0.6*ftu
            newlai(8)=newlai(8)+vfrac(i,j,n)*0.6*ftu*vlai(i,j,n)
          case (9)
            if (abs(clat)>40.5) then
              newgrid(1)=newgrid(1)+vfrac(i,j,n)*0.1
              newlai(1)=newlai(1)+vfrac(i,j,n)*0.1*vlai(i,j,n)
            else if (abs(clat)>39.5) then
              xp=abs(clat)-39.5
              newgrid(1)=newgrid(1)+vfrac(i,j,n)*0.1*xp
              newlai(1)=newlai(1)+vfrac(i,j,n)*vlai(i,j,n)*0.1*xp
              savannafrac(i,j)=savannafrac(i,j)+vfrac(i,j,n)*0.1*(1.-xp)
              newgrid(2)=newgrid(2)+vfrac(i,j,n)*0.1*(1.-xp)
              newlai(2)=newlai(2)+vfrac(i,j,n)*vlai(i,j,n)*0.1*(1.-xp)
            else
              savannafrac(i,j)=savannafrac(i,j)+vfrac(i,j,n)*0.1
              newgrid(2)=newgrid(2)+vfrac(i,j,n)*0.1
              newlai(2)=newlai(2)+vfrac(i,j,n)*0.1*vlai(i,j,n)
            end if
            newgrid(6)=newgrid(6)+vfrac(i,j,n)*0.9*fg3
            newlai(6)=newlai(6)+vfrac(i,j,n)*0.9*fg3*vlai(i,j,n)
            newgrid(7)=newgrid(7)+vfrac(i,j,n)*0.9*fg4
            newlai(7)=newlai(7)+vfrac(i,j,n)*0.9*fg4*vlai(i,j,n)
            newgrid(8)=newgrid(8)+vfrac(i,j,n)*0.9*ftu
            newlai(8)=newlai(8)+vfrac(i,j,n)*0.9*ftu*vlai(i,j,n)
          case (10)
            newgrid(6)=newgrid(6)+vfrac(i,j,n)*fg3
            newlai(6)=newlai(6)+vfrac(i,j,n)*fg3*vlai(i,j,n)
            newgrid(7)=newgrid(7)+vfrac(i,j,n)*fg4
            newlai(7)=newlai(7)+vfrac(i,j,n)*fg4*vlai(i,j,n)
            newgrid(8)=newgrid(8)+vfrac(i,j,n)*ftu
            newlai(8)=newlai(8)+vfrac(i,j,n)*ftu*vlai(i,j,n)
          case (12,14)
            newgrid(9)=newgrid(9)+vfrac(i,j,n)*fc3
            newlai(9)=newlai(9)+vfrac(i,j,n)*fc3*vlai(i,j,n)
            newgrid(10)=newgrid(10)+vfrac(i,j,n)*fc4
            newlai(10)=newlai(10)+vfrac(i,j,n)*fc4*vlai(i,j,n)
          case (13)
            newgrid(15)=newgrid(15)+vfrac(i,j,n)
            newlai(15)=newlai(15)+vfrac(i,j,n)*vlai(i,j,n)
          case (15)
            newgrid(17)=newgrid(17)+vfrac(i,j,n)
            newlai(17)=newlai(17)+vfrac(i,j,n)*vlai(i,j,n)
          case (16)
            newgrid(14)=newgrid(14)+vfrac(i,j,n)
            newlai(14)=newlai(14)+vfrac(i,j,n)*vlai(i,j,n)
          case (17)
            newgrid(16)=newgrid(16)+vfrac(i,j,n)
            newlai(16)=newlai(16)+vfrac(i,j,n)*vlai(i,j,n)
          case DEFAULT
            write(6,*) "ERROR: Land-type/lsmask mismatch at i,j,vtype,land=",i,j,vtype(i,j,n),lsdata(i,j)
            stop
        end select
      end do
      if (newgrid(2)>0.) then
        savannafrac(i,j)=savannafrac(i,j)/newgrid(2)
      end if
      where ( newgrid(:)>0. )
        newlai(:) = newlai(:)/newgrid(:)
      end where
      ipos = count(newgrid(:)>0.)
      do while ( ipos>5 )
        pos = minloc(newgrid(:), newgrid(:)>0.)
        newgrid(pos(1)) = 0.
        nsum = sum(newgrid(:))
        newgrid(:) = newgrid(:)/nsum
        ipos = count(newgrid(:)>0.)
      end do    
      do while ( any(newgrid(:)<minfrac.and.newgrid(:)>0.) )
        pos = minloc(newgrid(:), newgrid(:)>0.)
        newgrid(pos(1)) = 0.
        nsum = sum(newgrid(:))
        newgrid(:) = newgrid(:)/nsum
      end do

      n = 0
      vtype(i,j,:) = 0
      vfrac(i,j,:) = 0.
      vlai(i,j,:)  = 0.
      do iv = 1,mxvt
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
    