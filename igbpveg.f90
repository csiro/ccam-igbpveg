Program igbpveg

! This code creates CCAM vegie data using the 1km SiB dataset

Implicit None

Character*80, dimension(:,:), allocatable :: options
Character*80, dimension(1:7) :: fname
Character*80 topofile,albvisout,albnirout
Character*80 soilout,landtypeout
Character*80 newtopofile,urbanout
Integer binlimit, nopts, month
Logical fastigbp,igbplsmask,ozlaipatch,tile

Namelist/vegnml/ topofile,albvisout,albnirout, &
                 soilout,fastigbp, &
                 landtypeout,igbplsmask,newtopofile, &
                 binlimit,urbanout,month,ozlaipatch, &
                 tile

Write(6,*) 'IGBPVEG - IGBP 1km to CC grid (MAY-10)'

! Read switches
nopts=1
Allocate (options(nopts,2))
options(:,1) = (/ '-s' /)
options(:,2) = ''

Call readswitch(options,nopts)
Call defaults(options,nopts)

! Read namelist
Write(6,*) 'Input &vegnml namelist'
Read(5,NML=vegnml)
Write(6,*) 'Namelist accepted'

! Generate veg data
fname(1)=topofile
fname(2)=soilout
fname(3)=albvisout
fname(4)=albnirout
fname(5)=landtypeout
fname(6)=urbanout
fname(7)=newtopofile

Call createveg(options,nopts,fname,fastigbp,igbplsmask,ozlaipatch,tile,month,binlimit)

Deallocate(options)

Stop
End

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
Write(6,*) '    newtopofile="topoutb"'
Write(6,*) '    soilout="soil"'
Write(6,*) '    albvisout="albvis"'
Write(6,*) '    albnirout="albnir"'
Write(6,*) '    urbanout="urban"'
Write(6,*) '    landtypeout="veg"'
Write(6,*) '    fastigbp=t'
Write(6,*) '    igbplsmask=t'
Write(6,*) '    ozlaipatch=f'
Write(6,*) '    tile=t'
Write(6,*) '    binlimit=2'
Write(6,*) '  &end'
Write(6,*)
Write(6,*) '  where:'
Write(6,*) '    month         = the month to process (1-12, 0=all)'
Write(6,*) '    topofile      = topography (input) file'
Write(6,*) '    newtopofile   = Output topography file name'
Write(6,*) '                    (if igbplsmask=t)'
Write(6,*) '    soilout       = Soil filename (Zobler)'
Write(6,*) '    albvisout     = Soil albedo (VIS) filename'
Write(6,*) '    albnirout     = Soil albedo (NIR) filename'
Write(6,*) '    urbanout      = Urban cover fraction filename'
Write(6,*) '    landtypeout   = Land-use classification filename'
Write(6,*) '    fastigbp      = Turn on fastigbp mode (see notes below)'
Write(6,*) '    igbplsmask    = Define land/sea mask from IGBP dataset'
Write(6,*) '    ozlaipath     = Use CSIRO LAI dataset for Australia'
Write(6,*) '    tile          = Seperate land cover into tiles'
Write(6,*) '    binlimit      = The minimum ratio between the grid'
Write(6,*) '                    length scale and the length scale of'
Write(6,*) '                    the aggregated land-use data (see notes'
Write(6,*) '                    below).'
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

If (options(siz,2).EQ.'') then
  options(siz,2)='500'
End if

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine processes the sib data
!

Subroutine createveg(options,nopts,fname,fastigbp,igbplsmask,ozlaipatch,tile,month,binlimit)

Use ccinterp

Implicit None

Logical, intent(in) :: fastigbp,igbplsmask,ozlaipatch,tile
Integer, intent(in) :: nopts,binlimit,month
Character(len=*), dimension(1:nopts,1:2), intent(in) :: options
Character(len=*), dimension(1:7), intent(in) :: fname
Character*80, dimension(1:3) :: outputdesc
Character*80 returnoption,csize,filedesc
Character*45 header
Character*9 formout
Character*2 monthout
real, dimension(:,:,:,:), allocatable :: vlai
Real, dimension(:,:,:), allocatable :: landdata,soildata,rlld,rdata,vfrac
Real, dimension(:,:), allocatable :: gridout,lsdata,urbandata,oceandata,albvisdata,albnirdata
Real, dimension(1:3,1:2) :: alonlat
Real, dimension(1:2) :: lonlat
Real, dimension(1:12) :: atime
Real, dimension(1) :: alvl
Real schmidt,dsx,ds,urbanfrac
integer, dimension(:,:), allocatable :: idata
integer, dimension(:,:,:), allocatable :: vtype
Integer, dimension(1:2) :: sibdim
Integer, dimension(1:4) :: dimnum,dimid,dimcount
Integer, dimension(0:4) :: ncidarr
Integer, dimension(1:6) :: adate
Integer, dimension(2:20) :: varid
Integer sibsize,tunit,i,j,k,ierr,sibmax(1),mthrng
logical, dimension(1:16) :: sermsk

mthrng=1
if (month.eq.0) then
  mthrng=12
end if
if ((month.lt.0).or.(month.gt.12)) then
  write(6,*) "ERROR: Invalid month ",month
  stop
end if

csize=returnoption('-s',options,nopts)
Read(csize,FMT=*,IOSTAT=ierr) sibsize
If (ierr.NE.0) then
  Write(6,*) 'ERROR: Invalid array size.  Must be an integer.'
  Stop
End if

! Read topography file
tunit=1
Call readtopography(tunit,fname(1),sibdim,lonlat,schmidt,dsx,header)
Write(6,*) "Dimension : ",sibdim
Write(6,*) "lon0,lat0 : ",lonlat
Write(6,*) "Schmidt   : ",schmidt
Allocate(gridout(1:sibdim(1),1:sibdim(2)),rlld(1:sibdim(1),1:sibdim(2),1:2))
Allocate(albvisdata(1:sibdim(1),1:sibdim(2)),oceandata(1:sibdim(1),1:sibdim(2)))
Allocate(albnirdata(1:sibdim(1),1:sibdim(2)))
Allocate(soildata(1:sibdim(1),1:sibdim(2),0:8),lsdata(1:sibdim(1),1:sibdim(2)))
Allocate(urbandata(1:sibdim(1),1:sibdim(2)),landdata(1:sibdim(1),1:sibdim(2),0:17+16*mthrng))

! Determine lat/lon to CC mapping
Call ccgetgrid(rlld,gridout,sibdim,lonlat,schmidt,ds)

! Read sib data
Call getdata(landdata,lonlat,gridout,rlld,sibdim,17+16*mthrng,sibsize,'land',fastigbp,ozlaipatch,binlimit,month)
Call getdata(soildata,lonlat,gridout,rlld,sibdim,8,sibsize,'soil',fastigbp,ozlaipatch,binlimit,month)
Call getdata(albvisdata,lonlat,gridout,rlld,sibdim,0,sibsize,'albvis',fastigbp,ozlaipatch,binlimit,month)
Call getdata(albnirdata,lonlat,gridout,rlld,sibdim,0,sibsize,'albnir',fastigbp,ozlaipatch,binlimit,month)

write(6,*) "Preparing data..."
! extract urban cover
urbandata(:,:)=landdata(:,:,13)

! remove IGBP urban classes 13
Call igbpfix(landdata,rlld,sibdim,mthrng)

if (igbplsmask) then
  write(6,*) "Using IGBP land/sea mask"
  where ((landdata(:,:,0)+landdata(:,:,17)).gt.0.)
    oceandata=landdata(:,:,0)/(landdata(:,:,0)+landdata(:,:,17))
  else where
    oceandata=0.
  end where
  lsdata=real(nint(landdata(:,:,0)+landdata(:,:,17)))
  call cleantopo(tunit,fname(1),fname(7),lsdata,oceandata,sibdim)
else
  write(6,*) "Using topography land/sea mask"
  call gettopols(tunit,fname(1),lsdata,sibdim)
  oceandata=lsdata
end if

urbandata=min(urbandata,(1.-lsdata))

! Clean-up soil, lai, veg, albedo and urban data
Call cleanigbp(landdata,lsdata,rlld,sibdim,mthrng)
Call cleanreal(soildata,8,lsdata,rlld,sibdim)
Call cleanreal(albvisdata,0,lsdata,rlld,sibdim)
Call cleanreal(albnirdata,0,lsdata,rlld,sibdim)

Deallocate(gridout,oceandata)
Allocate(rdata(1:sibdim(1),1:sibdim(2),1:mthrng),idata(1:sibdim(1),1:sibdim(2)))
Allocate(vfrac(1:sibdim(1),1:sibdim(2),1:5),vtype(1:sibdim(1),1:sibdim(2),1:5))
allocate(vlai(1:sibdim(1),1:sibdim(2),1:5,1:mthrng))

Call calsoilnear(landdata,soildata,lsdata,sibdim,idata)
where (lsdata(:,:).ge.0.5)
  albvisdata(:,:)=0.08 ! 0.07 in Masson (2003)
  albnirdata(:,:)=0.08 ! 0.20 in Masson (2003)
else where (idata.eq.9)
  albvisdata(:,:)=0.80
  albnirdata(:,:)=0.40
end where

! Prep nc output
dimnum(1:2)=sibdim(1:2) ! CC grid dimensions
dimnum(3)=1 ! Turn off level
dimnum(4)=mthrng ! Number of months in a year
adate=0 ! Turn off date
adate(2)=1 ! time units=months
Call ncinitcc(ncidarr,'veg.nc',dimnum(1:3),dimid,adate)
outputdesc=(/ 'soilt', 'Soil classification', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(2),1.,0.)
outputdesc=(/ 'albvis', 'Soil albedo (VIS)', '' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(3),1.,0.)
outputdesc=(/ 'albnir', 'Soil albedo (NIR)', '' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(4),1.,0.)
outputdesc=(/ 'lai1', 'Leaf Area Index (tile1)', '' /)
Call ncaddvargen(ncidarr,outputdesc,5,3,varid(5),1.,0.)
outputdesc=(/ 'vegt1', 'Land-use classification (tile1)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(6),1.,0.)
outputdesc=(/ 'urban', 'Urban fraction', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(7),1.,0.)
outputdesc=(/ 'vegt2', 'Land-use classification (tile2)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(8),1.,0.)
outputdesc=(/ 'vegt3', 'Land-use classification (tile3)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(9),1.,0.)
outputdesc=(/ 'vegt4', 'Land-use classification (tile4)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(10),1.,0.)
outputdesc=(/ 'vegt5', 'Land-use classification (tile5)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(11),1.,0.)
outputdesc=(/ 'vfrac1', 'Land-use cover fraction (tile1)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(12),1.,0.)
outputdesc=(/ 'vfrac2', 'Land-use cover fraction (tile2)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(13),1.,0.)
outputdesc=(/ 'vfrac3', 'Land-use cover fraction (tile3)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(14),1.,0.)
outputdesc=(/ 'vfrac4', 'Land-use cover fraction (tile4)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(15),1.,0.)
outputdesc=(/ 'vfrac5', 'Land-use cover fraction (tile5)', 'none' /)
Call ncaddvargen(ncidarr,outputdesc,5,2,varid(16),1.,0.)
outputdesc=(/ 'lai2', 'Leaf Area Index (tile2)', '' /)
Call ncaddvargen(ncidarr,outputdesc,5,3,varid(17),1.,0.)
outputdesc=(/ 'lai3', 'Leaf Area Index (tile3)', '' /)
Call ncaddvargen(ncidarr,outputdesc,5,3,varid(18),1.,0.)
outputdesc=(/ 'lai4', 'Leaf Area Index (tile4)', '' /)
Call ncaddvargen(ncidarr,outputdesc,5,3,varid(19),1.,0.)
outputdesc=(/ 'lai5', 'Leaf Area Index (tile5)', '' /)
Call ncaddvargen(ncidarr,outputdesc,5,3,varid(20),1.,0.)
Call ncenddef(ncidarr)
alonlat(:,1)=(/ 1., real(sibdim(1)), 1. /)
alonlat(:,2)=(/ 1., real(sibdim(2)), 1. /)
alvl=1.
if (mthrng.eq.12) then
  Do i=1,12
    atime(i)=Real(i) ! Define Months
  End do
else
  atime(1)=real(month)
end if
Call nclonlatgen(ncidarr,dimid,alonlat,alvl,atime,dimnum)


! Write soil type
Write(6,*) 'Write soil type file.'
Write(formout,'(1h(,i3,2hi3,1h))') sibdim(1)
Open(1,File=fname(2))
Write(1,'(i3,i4,2f8.3,f6.3,f8.0," ",a39)') sibdim(1),sibdim(2),lonlat(1),lonlat(2),schmidt,ds,'soil'
Write(1,formout) idata
Close(1)
dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
Call ncwritedatgen(ncidarr,Real(idata),dimcount,varid(2))

! Write albedo file
Write(6,*) 'Write albedo files.'
Write(formout,'("(",i3,"f4.0)" )') sibdim(1)
Open(1,File=fname(3))
Write(1,'(i3,i4,2f8.3,f6.3,f8.0," ",a39)') sibdim(1),sibdim(2),lonlat(1),lonlat(2),schmidt,ds,'soilalbvis'
Write(1,formout) max(min(albvisdata(:,:)*100.,99.),1.)
Close(1)
dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
Call ncwritedatgen(ncidarr,albvisdata,dimcount,varid(3))

Write(formout,'("(",i3,"f4.0)" )') sibdim(1)
Open(1,File=fname(4))
Write(1,'(i3,i4,2f8.3,f6.3,f8.0," ",a39)') sibdim(1),sibdim(2),lonlat(1),lonlat(2),schmidt,ds,'soilalbnir'
Write(1,formout) max(min(albnirdata(:,:)*100.,99.),1.)
Close(1)
dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
Call ncwritedatgen(ncidarr,albnirdata,dimcount,varid(4))

write(6,*) 'Write land-use type'
Do j=1,sibdim(2)
  Do i=1,sibdim(1)
    if (lsdata(i,j).ge.0.5) then
      vtype(i,j,1:5)=0
      vfrac(i,j,1)=1.
      vfrac(i,j,2:5)=0.
      vlai(i,j,:,:)=0.
    else
      sermsk=.true.
      do k=1,5
        sibmax=Maxloc(landdata(i,j,1:16),sermsk)
        sermsk(sibmax(1))=.false.
        vtype(i,j,k)=sibmax(1)
        vfrac(i,j,k)=landdata(i,j,sibmax(1))
        vlai(i,j,k,1:mthrng)=landdata(i,j,17+(sibmax(1)-1)*mthrng+1:17+(sibmax(1)-1)*mthrng+mthrng)
      end do
      if (.not.tile) then
        do k=1,mthrng
          vlai(i,j,1,k)=sum(vlai(i,j,:,k)*vfrac(i,j,:))/sum(vfrac(i,j,:))
        end do
        vlai(i,j,2:5,1:mthrng)=0.
        vfrac(i,j,1)=1.
        vfrac(i,j,2:5)=0.        
      end if
      vfrac(i,j,:)=vfrac(i,j,:)/sum(vfrac(i,j,:))
      vfrac(i,j,1:4)=real(nint(100.*vfrac(i,j,1:4)))/100.
      vfrac(i,j,5)=1.-sum(vfrac(i,j,1:4))
      do k=5,2,-1
        if ((vfrac(i,j,k).lt.0.).or.(vfrac(i,j,k-1).lt.vfrac(i,j,k))) then
          vfrac(i,j,k-1)=vfrac(i,j,k-1)+vfrac(i,j,k)
          vfrac(i,j,k)=0.
        end if
      end do
    end if
  End do
End do
do k=1,mthrng
  if (mthrng.eq.12) then
    Write(monthout,'(I2.2)') k
    filedesc=trim(fname(5))//'.'//trim(monthout)
    open(1,file=filedesc)
  else
    open(1,File=fname(5))
  end if
  write(1,'(i3,i4,2f8.3,f6.3,f8.0," ",a39)') sibdim(1),sibdim(2),lonlat(1),lonlat(2),schmidt,ds,'land'
  Do j=1,sibdim(2)
    Do i=1,sibdim(1)
      write(1,'(I10,F8.2,F7.2,I3,2F6.2,I3,2F6.2,I3,2F6.2,I3,2F6.2,I3,2F6.2)') &
          i+(j-1)*sibdim(1),rlld(i,j,1),rlld(i,j,2), &
          vtype(i,j,1),vfrac(i,j,1),vlai(i,j,1,k), &
          vtype(i,j,2),vfrac(i,j,2),vlai(i,j,2,k), &
          vtype(i,j,3),vfrac(i,j,3),vlai(i,j,3,k), &
          vtype(i,j,4),vfrac(i,j,4),vlai(i,j,4,k), &
          vtype(i,j,5),vfrac(i,j,5),vlai(i,j,5,k)
    End do
  End do
  Close(1)
end do
dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
Call ncwritedatgen(ncidarr,Real(vtype(:,:,1)),dimcount,varid(6))
Call ncwritedatgen(ncidarr,Real(vtype(:,:,2)),dimcount,varid(8))
Call ncwritedatgen(ncidarr,Real(vtype(:,:,3)),dimcount,varid(9))
Call ncwritedatgen(ncidarr,Real(vtype(:,:,4)),dimcount,varid(10))
Call ncwritedatgen(ncidarr,Real(vtype(:,:,5)),dimcount,varid(11))
Call ncwritedatgen(ncidarr,vfrac(:,:,1),dimcount,varid(12))
Call ncwritedatgen(ncidarr,vfrac(:,:,2),dimcount,varid(13))
Call ncwritedatgen(ncidarr,vfrac(:,:,3),dimcount,varid(14))
Call ncwritedatgen(ncidarr,vfrac(:,:,4),dimcount,varid(15))
Call ncwritedatgen(ncidarr,vfrac(:,:,5),dimcount,varid(16))
dimcount=(/ sibdim(1), sibdim(2), mthrng, 1 /)
Call ncwritedatgen(ncidarr,vlai(:,:,1,:),dimcount,varid(5))
Call ncwritedatgen(ncidarr,vlai(:,:,2,:),dimcount,varid(17))
Call ncwritedatgen(ncidarr,vlai(:,:,3,:),dimcount,varid(18))
Call ncwritedatgen(ncidarr,vlai(:,:,4,:),dimcount,varid(19))
Call ncwritedatgen(ncidarr,vlai(:,:,5,:),dimcount,varid(20))

! Urban
Write(6,*) 'Write urban fraction'
urbanfrac=1.0 ! veg now included in urban scheme
Write(formout,'("(",i3,"f5.0)" )') sibdim(1)
Open(1,File=fname(6))
Write(1,'(i3,i4,2f8.3,f6.3,f8.0," ",a39)') sibdim(1),sibdim(2),lonlat(1),lonlat(2),schmidt,ds,'urban'
Write(1,formout) urbandata(:,:)*urbanfrac*100.
Close(1)
dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
Call ncwritedatgen(ncidarr,urbandata*urbanfrac,dimcount,varid(7))

Call ncclose(ncidarr)

Deallocate(landdata,soildata,rdata,urbandata,lsdata)
Deallocate(vfrac,vtype,rlld,idata,vlai)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Fix IGBP data
!

subroutine igbpfix(landdata,rlld,sibdim,mthrng)

implicit none

integer, intent(in) :: mthrng
integer, dimension(1:2), intent(in) :: sibdim
real, dimension(sibdim(1),sibdim(2),1:2), intent(in) :: rlld
real, dimension(1:sibdim(1),1:sibdim(2),0:17+16*mthrng), intent(inout) :: landdata
real, dimension(1:sibdim(1),1:sibdim(2),0:17+16*mthrng) :: landtemp
logical, dimension(1:sibdim(1),1:sibdim(2)) :: sermsk
integer i,ilon,ilat,pxy(2)
real nsum,wsum

landdata(:,:,13)=0. ! remove urban
landtemp=landdata

sermsk=sum(landdata(:,:,1:16),3).gt.0.
if (.not.any(sermsk)) return

do ilon=1,sibdim(1)
  do ilat=1,sibdim(2)
    wsum=landdata(ilon,ilat,0)+landdata(ilon,ilat,17) ! water
    if (wsum.lt.1.) then
      nsum=sum(landdata(ilon,ilat,1:16)) ! land
      if (nsum.le.0.) then
        call findnear(pxy,ilon,ilat,sermsk,rlld,sibdim)
        landdata(ilon,ilat,1:16)=landtemp(pxy(1),pxy(2),1:16)  
        landdata(ilon,ilat,18:)=landtemp(pxy(1),pxy(2),18:)
        nsum=sum(landdata(ilon,ilat,1:16))
      end if
      landdata(ilon,ilat,1:16)=landdata(ilon,ilat,1:16)*max(1.-wsum,0.)/nsum
    end if
  end do
end do

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! clean land data
!

subroutine cleanigbp(dataout,lsdata,rlld,sibdim,mthrng)

implicit none

integer, intent(in) :: mthrng
integer, dimension(1:2), intent(in) :: sibdim
real, dimension(1:sibdim(1),1:sibdim(2),0:17+16*mthrng), intent(inout) :: dataout
real, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: lsdata
real, dimension(1:sibdim(1),1:sibdim(2),1:2), intent(in) :: rlld
real, dimension(1:sibdim(1),1:sibdim(2),0:17+16*mthrng) :: datain
logical, dimension(1:sibdim(1),1:sibdim(2)) :: sermsk,ocnmsk
integer ilon,ilat,pxy(2)
real nsum,wsum

datain=dataout
sermsk=sum(datain(:,:,1:16),3).gt.0.
ocnmsk=(datain(:,:,0)+datain(:,:,17)).gt.0.
if (.not.any(sermsk)) then
  dataout(:,:,0)=1.
  dataout(:,:,1:)=0.
  return
end if

do ilon=1,sibdim(1)
  do ilat=1,sibdim(2)
    if (1-nint(lsdata(ilon,ilat)).eq.1) then
      if (.not.sermsk(ilon,ilat)) then
        call findnear(pxy,ilon,ilat,sermsk,rlld,sibdim)
        dataout(ilon,ilat,1:16)=datain(pxy(1),pxy(2),1:16)
        dataout(ilon,ilat,18:)=datain(pxy(1),pxy(2),18:)
      end if
      nsum=sum(dataout(ilon,ilat,1:16))
      dataout(ilon,ilat,1:16)=dataout(ilon,ilat,1:16)*max(1.-lsdata(ilon,ilat),0.)/nsum
    else
      dataout(ilon,ilat,1:16)=0.
      dataout(ilon,ilat,18:)=0.
    end if
    if (1-nint(lsdata(ilon,ilat)).eq.0) then
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
integer, dimension(1:2), intent(in) :: sibdim
real, dimension(1:sibdim(1),1:sibdim(2),0:num), intent(inout) :: dataout
real, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: lsdata
real, dimension(1:sibdim(1),1:sibdim(2),1:2), intent(in) :: rlld
real, dimension(1:sibdim(1),1:sibdim(2),0:num) :: datain
logical, dimension(1:sibdim(1),1:sibdim(2)) :: sermsk
integer ilon,ilat,pxy(2)
real nsum

datain=dataout

sermsk=.true.
do ilon=0,num
  sermsk=sermsk.and.(datain(:,:,ilon).gt.0.)
end do
if (.not.any(sermsk)) return

do ilon=1,sibdim(1)
  do ilat=1,sibdim(2)
    if ((1-nint(lsdata(ilon,ilat)).eq.1).and.(.not.sermsk(ilon,ilat))) then
      call findnear(pxy,ilon,ilat,sermsk,rlld,sibdim)
      dataout(ilon,ilat,:)=datain(pxy(1),pxy(2),:)
    end if
  end do
end do

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine rewrites the land/sea mask in the topography
! data file.
!

Subroutine cleantopo(topounit,toponame,topoout,lsmskin,oceanin,sibdim)

Implicit None

Integer, intent(in) :: topounit
Integer, dimension(1:2), intent(in) :: sibdim
Integer ilout,ierr,ia,ib
Character(len=*), intent(in) :: toponame,topoout
Character*80 formout
Character*47 dc
Real, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: lsmskin,oceanin
Real, dimension(1:sibdim(1),1:sibdim(2)) :: topo,sd,lsmsk
Real ra,rb,rc,rd
ilout=Min(sibdim(1),30) ! To be compatiable with terread

Write(6,*) "Adjust topography data for consistancy with land-sea mask"

Open(topounit,FILE=toponame,FORM='formatted',STATUS='old',IOSTAT=ierr)
Read(topounit,*,IOSTAT=ierr) ia,ib,ra,rb,rc,rd,dc
Read(topounit,*,IOSTAT=ierr) topo ! Topography data
Read(topounit,*,IOSTAT=ierr) lsmsk ! land/sea mask (to be replaced)
Read(topounit,*,IOSTAT=ierr) sd ! Topography standard deviation
Close(topounit)

If (ierr.NE.0) then
  Write(6,*) "ERROR: Cannot read file ",trim(toponame)
  Stop
End if

lsmsk=Real(1-nint(lsmskin))
where ((nint(oceanin).eq.1).and.(nint(lsmskin).eq.1))
  topo(:,:)=0.
  sd(:,:)=0.
end where

Open(topounit,FILE=topoout,FORM='formatted',STATUS='replace',IOSTAT=ierr)
Write(topounit,'(i3,i4,2f10.3,f6.3,f8.0," ",a39)',IOSTAT=ierr) ia,ib,ra,rb,rc,rd,dc
Write(formout,'("(",i3,"f7.0)")',IOSTAT=ierr) ilout
Write(topounit,formout,IOSTAT=ierr) topo ! Topography data
Write(formout,'("(",i3,"f4.1)")',IOSTAT=ierr) ilout
Write(topounit,formout,IOSTAT=ierr) lsmsk ! land/sea mask
Write(formout,'("(",i3,"f6.0)")',IOSTAT=ierr) ilout
Write(topounit,formout,IOSTAT=ierr) sd ! Topography standard deviation
Close(topounit)

If (ierr.NE.0) then
  Write(6,*) "ERROR: Cannot write file ",trim(toponame)
  Stop
End if

Return
End
