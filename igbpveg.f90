Program igbpveg

! This code creates CCAM vegie data using the 1km SiB dataset

Implicit None

character*80, dimension(:,:), allocatable :: options
character*80, dimension(3) :: fname
character*80 topofile
character*80 landtypeout
character*80 newtopofile
integer binlimit, nopts, month
logical fastigbp,igbplsmask,ozlaipatch,tile

namelist/vegnml/ topofile,fastigbp,                  &
                 landtypeout,igbplsmask,newtopofile, &
                 binlimit,month,ozlaipatch,          &
                 tile

write(6,*) 'IGBPVEG - IGBP 1km to CC grid (FEB-13)'

! Read switches
nopts=1
allocate (options(nopts,2))
options(:,1) = (/ '-s' /)
options(:,2) = ''

call readswitch(options,nopts)
call defaults(options,nopts)

! Read namelist
write(6,*) 'Input &vegnml namelist'
read(5,NML=vegnml)
write(6,*) 'Namelist accepted'

! Generate veg data
fname(1)=topofile
fname(2)=landtypeout
fname(3)=newtopofile

call createveg(options,nopts,fname,fastigbp,igbplsmask,ozlaipatch,tile,month,binlimit)

deallocate(options)

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
Write(6,*) '    newtopofile="topoutb"'
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
Write(6,*) '    landtypeout   = Land-use filename'
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

If (options(siz,2)=='') then
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
Character(len=*), dimension(nopts,2), intent(in) :: options
Character(len=*), dimension(3), intent(in) :: fname
character*90 filename
Character*80, dimension(1:3) :: outputdesc
Character*80 returnoption,csize,filedesc
Character*45 header
Character*9 formout
Character*2 monthout
real, dimension(:,:,:), allocatable :: vlai
Real, dimension(:,:,:), allocatable :: landdata,soildata,rlld,rdata,vfrac,tmp
Real, dimension(:,:), allocatable :: gridout,lsdata,urbandata,oceandata,albvisdata,albnirdata
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
Integer, dimension(2:20) :: varid
Integer sibsize,tunit,i,j,k,ierr,sibmax(1),mthrng
integer tt
logical, dimension(16) :: sermsk

mthrng=1
if (month==0) then
  mthrng=12
end if
if ((month<0).or.(month>12)) then
  write(6,*) "ERROR: Invalid month ",month
  write(6,*) "Must be between 0 and 12"
  stop
end if

csize=returnoption('-s',options,nopts)
read(csize,FMT=*,IOSTAT=ierr) sibsize
if (ierr.NE.0) then
  write(6,*) 'ERROR: Invalid array size.  Must be an integer.'
  stop
end if

! Read topography file
tunit=1
call readtopography(tunit,fname(1),sibdim,lonlat,schmidt,dsx,header)

write(6,*) "Dimension : ",sibdim
write(6,*) "lon0,lat0 : ",lonlat
write(6,*) "Schmidt   : ",schmidt
allocate(gridout(1:sibdim(1),1:sibdim(2)),rlld(1:sibdim(1),1:sibdim(2),1:2))
allocate(albvisdata(1:sibdim(1),1:sibdim(2)),oceandata(1:sibdim(1),1:sibdim(2)))
allocate(albnirdata(1:sibdim(1),1:sibdim(2)))
allocate(soildata(1:sibdim(1),1:sibdim(2),0:8),lsdata(1:sibdim(1),1:sibdim(2)))
allocate(urbandata(1:sibdim(1),1:sibdim(2)),landdata(1:sibdim(1),1:sibdim(2),0:17+16*mthrng))

! Determine lat/lon to CC mapping
call ccgetgrid(rlld,gridout,sibdim,lonlat,schmidt,ds)

! Read sib data
call getdata(landdata,lonlat,gridout,rlld,sibdim,17+16*mthrng,sibsize,'land',fastigbp,ozlaipatch,binlimit,month)
call getdata(soildata,lonlat,gridout,rlld,sibdim,8,sibsize,'soil',fastigbp,ozlaipatch,binlimit,month)
call getdata(albvisdata,lonlat,gridout,rlld,sibdim,0,sibsize,'albvis',fastigbp,ozlaipatch,binlimit,month)
call getdata(albnirdata,lonlat,gridout,rlld,sibdim,0,sibsize,'albnir',fastigbp,ozlaipatch,binlimit,month)

write(6,*) "Preparing data..."
! extract urban cover and remove from landdata
urbandata(:,:)=landdata(:,:,13)
call igbpfix(landdata,rlld,sibdim,mthrng)

if (igbplsmask) then
  write(6,*) "Using IGBP land/sea mask"
  where ((landdata(:,:,0)+landdata(:,:,17))>0.)
    oceandata=landdata(:,:,0)/(landdata(:,:,0)+landdata(:,:,17))
  else where
    oceandata=0.
  end where
  lsdata=real(nint(landdata(:,:,0)+landdata(:,:,17)))
  call cleantopo(tunit,fname(1),fname(3),lsdata,oceandata,sibdim)
else
  write(6,*) "Using topography land/sea mask"
  call gettopols(tunit,fname(1),lsdata,sibdim)
  oceandata=lsdata
end if


write(6,*) "clean urban data"
urbandata=min(urbandata,(1.-lsdata))

! Clean-up soil, lai, veg, albedo and urban data
write(6,*) "Clean landuse data"
call cleanigbp(landdata,lsdata,rlld,sibdim,mthrng,soildata,albvisdata,albnirdata)
write(6,*) "Clean soil data"
call cleanreal(soildata,8,lsdata,rlld,sibdim)
write(6,*) "Clean albedo data"
allocate(tmp(sibdim(1),sibdim(2),0:1))
tmp(:,:,0)=albvisdata
tmp(:,:,1)=albnirdata
call cleanreal(tmp,1,lsdata,rlld,sibdim)
albvisdata=tmp(:,:,0)
albnirdata=tmp(:,:,1)
deallocate(tmp)

deallocate(gridout,oceandata)
allocate(rdata(sibdim(1),sibdim(2),mthrng),idata(sibdim(1),sibdim(2)))
allocate(vfrac(sibdim(1),sibdim(2),5),vtype(sibdim(1),sibdim(2),5))
allocate(vlai(sibdim(1),sibdim(2),5))

write(6,*) "Calculate soil texture"
call calsoilnear(landdata,soildata,lsdata,sibdim,idata)
where (lsdata(:,:)>=0.5)
  albvisdata(:,:)=0.08 ! 0.07 in Masson (2003)
  albnirdata(:,:)=0.08 ! 0.20 in Masson (2003)
else where (idata==9)
  albvisdata(:,:)=0.80
  albnirdata(:,:)=0.40
end where

write(6,*) "Ceate output file"
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
  outputdesc=(/ 'soilt', 'Soil classification', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(2),1.,0.)
  outputdesc=(/ 'albvis', 'Soil albedo (VIS)', '' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(3),1.,0.)
  outputdesc=(/ 'albnir', 'Soil albedo (NIR)', '' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(4),1.,0.)
  outputdesc=(/ 'lai1', 'Leaf Area Index (tile1)', '' /)
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(5),1.,0.)
  outputdesc=(/ 'vegt1', 'Land-use classification (tile1)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(6),1.,0.)
  outputdesc=(/ 'vfrac1', 'Land-use cover fraction (tile1)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(12),1.,0.)

  outputdesc=(/ 'lai2', 'Leaf Area Index (tile2)', '' /)
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(17),1.,0.)
  outputdesc=(/ 'vegt2', 'Land-use classification (tile2)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(8),1.,0.)
  outputdesc=(/ 'vfrac2', 'Land-use cover fraction (tile2)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(13),1.,0.)

  outputdesc=(/ 'lai3', 'Leaf Area Index (tile3)', '' /)
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(18),1.,0.)
  outputdesc=(/ 'vegt3', 'Land-use classification (tile3)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(9),1.,0.)
  outputdesc=(/ 'vfrac3', 'Land-use cover fraction (tile3)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(14),1.,0.)

  outputdesc=(/ 'lai4', 'Leaf Area Index (tile4)', '' /)
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(19),1.,0.)
  outputdesc=(/ 'vegt4', 'Land-use classification (tile4)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(10),1.,0.)
  outputdesc=(/ 'vfrac4', 'Land-use cover fraction (tile4)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(15),1.,0.)

  outputdesc=(/ 'lai5', 'Leaf Area Index (tile5)', '' /)
  call ncaddvargen(ncidarr,outputdesc,5,3,varid(20),1.,0.)
  outputdesc=(/ 'vegt5', 'Land-use classification (tile5)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(11),1.,0.)
  outputdesc=(/ 'vfrac5', 'Land-use cover fraction (tile5)', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(16),1.,0.)

  outputdesc=(/ 'urban', 'Urban fraction', 'none' /)
  call ncaddvargen(ncidarr,outputdesc,5,2,varid(7),1.,0.)

  call ncatt(ncidarr,'lon0',lonlat(1))
  call ncatt(ncidarr,'lat0',lonlat(2))
  call ncatt(ncidarr,'schmidt',schmidt)

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
  call ncwritedatgen(ncidarr,Real(idata),dimcount,varid(2))

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
          sibmax=Maxloc(landdata(i,j,1:16),sermsk)
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
  dimcount=(/ sibdim(1), sibdim(2), 1, 1 /)
  call ncwritedatgen(ncidarr,Real(vtype(:,:,1)),dimcount,varid(6))
  call ncwritedatgen(ncidarr,Real(vtype(:,:,2)),dimcount,varid(8))
  call ncwritedatgen(ncidarr,Real(vtype(:,:,3)),dimcount,varid(9))
  call ncwritedatgen(ncidarr,Real(vtype(:,:,4)),dimcount,varid(10))
  call ncwritedatgen(ncidarr,Real(vtype(:,:,5)),dimcount,varid(11))
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
  call ncwritedatgen(ncidarr,urbandata*urbanfrac,dimcount,varid(7))

  call ncclose(ncidarr)

end do

deallocate(landdata,soildata,rdata,urbandata,lsdata)
deallocate(vfrac,vtype,rlld,idata,vlai)

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
real, dimension(1:sibdim(1),1:sibdim(2),0:17+16*mthrng), intent(inout) :: landdata
real, dimension(:,:,:), allocatable :: landtemp
logical, dimension(1:sibdim(1),1:sibdim(2)) :: sermsk
integer i,ilon,ilat,pxy(2)
real nsum,wsum

! this array is generally too large for the stack
allocate(landtemp(sibdim(1),sibdim(2),0:17+16*mthrng))

landdata(:,:,13)=0. ! remove urban
landtemp=landdata

sermsk=sum(landdata(:,:,1:16),3)>0.
if (.not.any(sermsk)) return

do ilon=1,sibdim(1)
  do ilat=1,sibdim(2)
    wsum=landdata(ilon,ilat,0)+landdata(ilon,ilat,17) ! water
    nsum=sum(landdata(ilon,ilat,1:16))                ! land
    if (nint(wsum)==0) then
      if (nsum<=0.) then
        call findnear(pxy,ilon,ilat,sermsk,rlld,sibdim)
        landdata(ilon,ilat,1:16)=landtemp(pxy(1),pxy(2),1:16)  
        landdata(ilon,ilat,18:)=landtemp(pxy(1),pxy(2),18:)
        nsum=sum(landdata(ilon,ilat,1:16))
      end if
      landdata(ilon,ilat,1:16)=landdata(ilon,ilat,1:16)*(1.-wsum)/nsum
    else
      if (nsum>0.) then
        landdata(ilon,ilat,1:16)=landdata(ilon,ilat,1:16)*(1.-wsum)/nsum
      else
        landdata(ilon,ilat,0)=landdata(ilon,ilat,0)/wsum
        landdata(ilon,ilat,17)=landdata(ilon,ilat,17)/wsum
      end if
    end if
  end do
  if (mod(ilon,10)==0.or.ilon==sibdim(1)) then
    write(6,*) "Searching ",ilon,"/",sibdim(1)
  end if
end do

deallocate (landtemp)

return
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! clean land data
!

subroutine cleanigbp(dataout,lsdata,rlld,sibdim,mthrng, &
                     soildata,visdata,nirdata)

implicit none

integer, intent(in) :: mthrng
integer, dimension(2), intent(in) :: sibdim
real, dimension(sibdim(1),sibdim(2),0:17+16*mthrng), intent(inout) :: dataout
real, dimension(sibdim(1),sibdim(2),0:8), intent(inout) :: soildata
real, dimension(sibdim(1),sibdim(2)), intent(inout) :: visdata
real, dimension(sibdim(1),sibdim(2)), intent(inout) :: nirdata
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

do ilon=1,sibdim(1)
  do ilat=1,sibdim(2)
    if (1-nint(lsdata(ilon,ilat))==1) then
      if (.not.sermsk(ilon,ilat)) then
        call findnear(pxy,ilon,ilat,sermsk,rlld,sibdim)
        dataout(ilon,ilat,1:16)=datain(pxy(1),pxy(2),1:16)
        dataout(ilon,ilat,18:)=datain(pxy(1),pxy(2),18:)
        if (sum(soildata(ilon,ilat,:))<=0.) then
          soildata(ilon,ilat,:)=soildata(pxy(1),pxy(2),:)
        end if
        if (visdata(ilon,ilat)<=0.) then
          visdata(ilon,ilat)=visdata(pxy(1),pxy(2))
        end if
        if (nirdata(ilon,ilat)<=0.) then
          nirdata(ilon,ilat)=nirdata(pxy(1),pxy(2))
        end if
      end if
      nsum=sum(dataout(ilon,ilat,1:16))
      dataout(ilon,ilat,1:16)=dataout(ilon,ilat,1:16)*max(1.-lsdata(ilon,ilat),0.)/nsum
    else
      dataout(ilon,ilat,1:16)=0.
      dataout(ilon,ilat,18:)=0.
    end if
    if (1-nint(lsdata(ilon,ilat))==0) then
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
  sermsk=sermsk.and.(datain(:,:,ilon)>0.)
end do
if (.not.any(sermsk)) return

do ilon=1,sibdim(1)
  do ilat=1,sibdim(2)
    if ((1-nint(lsdata(ilon,ilat))==1).and.(.not.sermsk(ilon,ilat))) then
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

include 'netcdf.inc'

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
Real ra,rb,rc,rd
ilout=Min(sibdim(1),30) ! To be compatiable with terread

Write(6,*) "Adjust topography data for consistancy with land-sea mask"

ierr=nf_open(toponame,nf_nowrite,ncid)
if (ierr==0) then
  lnctopo=1
  spos=1
  npos(1)=sibdim(1)
  npos(2)=sibdim(2)
  npos(3)=1
  ierr=nf_get_att_real(ncid,nf_global,'lon0',ra)
  ierr=nf_get_att_real(ncid,nf_global,'lat0',rb)
  ierr=nf_get_att_real(ncid,nf_global,'schmidt',rc)
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

If (ierr.NE.0) then
  Write(6,*) "ERROR: Cannot read file ",trim(toponame)
  Stop
End if

lsmsk=Real(1-nint(lsmskin))
where ((nint(oceanin)==1).and.(nint(lsmskin)==1))
  topo(:,:)=0.
  sd(:,:)=0.
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
  ierr=nf_put_vara_real(ncid,varid,spos(1),npos(1),dum)
  ierr=nf_inq_varid(ncid,'latitude',varid)
  ierr=nf_put_vara_real(ncid,varid,spos(2),npos(2),dum)
  ierr=nf_inq_varid(ncid,'time',varid)
  dum(1)=0.
  ierr=nf_put_vara_real(ncid,varid,spos(3),npos(3),dum(1))
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
