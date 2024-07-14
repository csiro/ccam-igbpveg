! Conformal Cubic Atmospheric Model
    
! Copyright 2015-2024 Commonwealth Scientific Industrial Research Organisation (CSIRO)
    
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
    
! This subroutine is to extract (in memory) data from the IGBP dataset.
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads the sib, soil, albedo or urban data and maps
! it to a CC grid.  Data is specified by datatype (e.g., datatype=land
! or datatype=soil).
!

Subroutine getdata(dataout,glonlat,grid,tlld,sibdim,num,sibsize,datatype,fastigbp,binlimit,month, &
                   year,datafilename,laifilename,class_num,mapjveg,mapwater)

use ccinterp
use netcdf_m

Implicit None

Integer, intent(in) :: sibsize,num,binlimit,month,year,class_num
Integer, dimension(2), intent(in) :: sibdim
Integer, dimension(sibdim(1),sibdim(2)) :: countn
Integer, dimension(1:2) :: lldim,lldim_x,llstore,pxy
integer, dimension(class_num), intent(in) :: mapjveg
integer, dimension(0:12) :: ncid, varid
integer, dimension(:,:,:), allocatable :: lcmap
Integer nscale,nscale_x,nface,subsec,mode,tmp
Integer i,j,k,lci,lcj,nx,ny,imth,mthrng,netcount
Integer basesize,scalelimit,minscale
integer ierr, jj
Character(len=*), intent(in) :: datatype, datafilename, laifilename
Character(len=10) fname
Real, dimension(sibdim(1),sibdim(2),0:num), intent(out) :: dataout
Real, dimension(sibdim(1),sibdim(2)), intent(in) :: grid
Real, dimension(sibdim(1),sibdim(2),2), intent(in) :: tlld
Real, dimension(sibdim(1),sibdim(2),2) :: rlld
Real, dimension(sibdim(1),sibdim(2)) :: zsum
real, dimension(0:num) :: newdata, newcover
Real, dimension(2), intent(in) :: glonlat
Real, dimension(:,:,:), allocatable :: coverout
Real, dimension(1:2) :: latlon
Real, dimension(1:2,1:2) :: sll
Real, dimension(1:2,1:2,0:num) :: covertemp
real, dimension(1:12) :: netlai
Real aglon,aglat,alci,alcj,serlon,serlat,slonn,slatx,elon,elat,tscale,baselon
Real ipol,callon,callat,indexlon,indexlat
Logical, intent(in) :: fastigbp
Logical, dimension(:,:), allocatable :: sermask
logical, dimension(:,:,:), allocatable :: sermask0,sermask2
logical, dimension(sibdim(1),sibdim(2)) :: ltest
logical, dimension(class_num), intent(in) :: mapwater
logical, dimension(0:12) :: ncfile

if ( month==0 ) then
  mthrng=12
else
  mthrng=1
end if

dataout=0.
countn=0

! Determine scale limits
nscale=9999

baselon=real(int(glonlat(1)-180.))
rlld=tlld
Do While (Any(rlld(:,:,1)<baselon))
  Where (rlld(:,:,1)<baselon)
    rlld(:,:,1)=rlld(:,:,1)+360.
  End where
End do
Do While (Any(rlld(:,:,1)>(baselon+360.)))
  Where (rlld(:,:,1)>(baselon+360.))
    rlld(:,:,1)=rlld(:,:,1)-360.
  End where
End do

Select Case(datatype)
  Case('land')
    Write(6,*) 'Process USGS land-use and mod15_BU LAI datasets.'
    scalelimit=10
    ierr = nf90_open(trim(datafilename)//'.nc',nf90_nowrite,ncid(0))
    if ( ierr==nf90_noerr ) then
      ncfile(0) = .true.
      write(6,*) "Found netcdf version ",trim(datafilename)
      ierr = nf90_inq_varid(ncid(0),"vegt",varid(0))
    else
      ierr = nf90_open(trim(datafilename),nf90_nowrite,ncid(0))
      if ( ierr==nf90_noerr ) then
        ncfile(0) = .true.
        write(6,*) "Found netcdf version ",trim(datafilename)
        ierr = nf90_inq_varid(ncid(0),"vegt",varid(0))
      else    
        ncfile(0) = .false.
        ! Must be compiled using 1 byte record lengths
        Open(10,FILE=datafilename,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=43200,STATUS='OLD')
      end if
    end if
    if (month==0) then
      do imth=1,mthrng
        write(fname,'("slai",I2.2,".img")') imth
        ierr = nf90_open(trim(trim(laifilename)//'/'//fname)//'.nc',nf90_nowrite,ncid(imth))
        if ( ierr==nf90_noerr ) then
          ncfile(imth) = .true.
          write(6,*) "Found netcdf version ",trim(trim(laifilename)//'/'//fname)
          ierr = nf90_inq_varid(ncid(imth),"LAI",varid(imth))
        else
          ierr = nf90_open(trim(trim(laifilename)//'/'//fname),nf90_nowrite,ncid(imth))
          if ( ierr==nf90_noerr ) then
            ncfile(imth) = .true.
            write(6,*) "Found netcdf version ",trim(trim(laifilename)//'/'//fname)
            ierr = nf90_inq_varid(ncid(imth),"LAI",varid(imth)) 
          else
            ncfile(imth) = .false.  
            open(10+imth,FILE=trim(laifilename)//'/'//fname,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=10800,STATUS='OLD')
          end if
        end if 
      end do
    else
      ierr = nf90_open(trim(laifilename)//'.nc',nf90_nowrite,ncid(1))
      if ( ierr==nf90_noerr ) then
        ncfile(1) = .true.
        write(6,*) "Found netcdf version ",trim(laifilename)
        ierr = nf90_inq_varid(ncid(1),"LAI",varid(1))
      else
        ierr = nf90_open(trim(laifilename),nf90_nowrite,ncid(1))
        if ( ierr==nf90_noerr ) then
          ncfile(1) = .true.
          write(6,*) "Found netcdf version ",trim(laifilename)
          ierr = nf90_inq_varid(ncid(1),"LAI",varid(1)) 
        else
          ncfile(1) = .false.  
          open(11,FILE=laifilename,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=10800,STATUS='OLD')  
        end if
      end if  
    end if
  Case('land2')
    Write(6,*) 'Process MODIS land-use and mod15_BU LAI datasets.'
    scalelimit=10
    ierr = nf90_open(trim(datafilename)//'.nc',nf90_nowrite,ncid(0))
    if ( ierr==nf90_noerr ) then
      ncfile(0) = .true.
      write(6,*) "Found netcdf version ",trim(datafilename)
      ierr = nf90_inq_varid(ncid(0),"landcover",varid(0))
    else
      ierr = nf90_open(trim(datafilename),nf90_nowrite,ncid(0))
      if ( ierr==nf90_noerr ) then
        ncfile(0) = .true.
        write(6,*) "Found netcdf version ",trim(datafilename)
        ierr = nf90_inq_varid(ncid(0),"landcover",varid(0))
      else    
        ncfile(0) = .false.
        write(6,*) "ERROR: Input land-use file not found ",trim(datafilename)
        call finishbanner
      end if
    end if
    if (month==0) then
      do imth=1,mthrng
        write(fname,'("slai",I2.2,".img")') imth
        ierr = nf90_open(trim(trim(laifilename)//'/'//fname)//'.nc',nf90_nowrite,ncid(imth))
        if ( ierr==nf90_noerr ) then
          ncfile(imth) = .true.
          write(6,*) "Found netcdf version ",trim(trim(laifilename)//'/'//fname)
          ierr = nf90_inq_varid(ncid(imth),"LAI",varid(imth))
        else
          ierr = nf90_open(trim(trim(laifilename)//'/'//fname),nf90_nowrite,ncid(imth))
          if ( ierr==nf90_noerr ) then
            ncfile(imth) = .true.
            write(6,*) "Found netcdf version ",trim(trim(laifilename)//'/'//fname)
            ierr = nf90_inq_varid(ncid(imth),"LAI",varid(imth)) 
          else
            ncfile(imth) = .false.  
            open(10+imth,FILE=trim(laifilename)//'/'//fname,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=10800,STATUS='OLD')
          end if
        end if 
      end do
    else
      ierr = nf90_open(trim(laifilename)//'.nc',nf90_nowrite,ncid(1))
      if ( ierr==nf90_noerr ) then
        ncfile(1) = .true.
        write(6,*) "Found netcdf version ",trim(laifilename)
        ierr = nf90_inq_varid(ncid(1),"LAI",varid(1))
      else
        ierr = nf90_open(trim(laifilename),nf90_nowrite,ncid(1))
        if ( ierr==nf90_noerr ) then
          ncfile(1) = .true.
          write(6,*) "Found netcdf version ",trim(laifilename)
          ierr = nf90_inq_varid(ncid(1),"LAI",varid(1)) 
        else
          ncfile(1) = .false.  
          open(11,FILE=laifilename,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=10800,STATUS='OLD')  
        end if
      end if  
    end if
  Case('soil')
    Write(6,*) 'Process HWSD soil dataset.'
    scalelimit=40
    ierr = nf90_open(trim(datafilename)//'.nc',nf90_nowrite,ncid(0))
    if ( ierr==nf90_noerr ) then
      ncfile(0) = .true.
      write(6,*) "Found netcdf version ",trim(datafilename)
      ierr = nf90_inq_varid(ncid(0),"soilt",varid(0))
    else  
      ierr = nf90_open(trim(datafilename),nf90_nowrite,ncid(0))
      if ( ierr==nf90_noerr ) then
        ncfile(0) = .true.
        write(6,*) "Found netcdf version ",trim(datafilename)
        ierr = nf90_inq_varid(ncid(0),"soilt",varid(0))    
      else
        ncfile(0) = .false.
        ! Must be compiled using 1 byte record lengths
        Open(20,FILE=datafilename,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=10800,STATUS='OLD')
      end if
    end if
  Case('albvis')
    Write(6,*) 'Process soil albedo (VIS) dataset.'
    scalelimit=40
    ierr = nf90_open(trim(datafilename)//'.nc',nf90_nowrite,ncid(0))
    if ( ierr==nf90_noerr ) then
      ncfile(0) = .true.
      write(6,*) "Found netcdf version ",trim(datafilename)
      ierr = nf90_inq_varid(ncid(0),datatype,varid(0))
    else  
      ierr = nf90_open(trim(datafilename),nf90_nowrite,ncid(0))
      if ( ierr==nf90_noerr ) then
        ncfile(0) = .true.
        write(6,*) "Found netcdf version ",trim(datafilename)
        ierr = nf90_inq_varid(ncid(0),datatype,varid(0))    
      else
        ncfile(0) = .false.
        ! Must be compiled using 1 byte record lengths
        Open(40,FILE=datafilename,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=10800,STATUS='OLD')
      end if
    end if 
  Case('albnir')
    Write(6,*) 'Process soil albedo (NIR) dataset.'
    scalelimit=40
    ierr = nf90_open(trim(datafilename)//'.nc',nf90_nowrite,ncid(0))
    if ( ierr==nf90_noerr ) then
      ncfile(0) = .true.
      write(6,*) "Found netcdf version ",trim(datafilename)
      ierr = nf90_inq_varid(ncid(0),datatype,varid(0))
    else  
      ierr = nf90_open(trim(datafilename),nf90_nowrite,ncid(0))
      if ( ierr==nf90_noerr ) then
        ncfile(0) = .true.
        write(6,*) "Found netcdf version ",trim(datafilename)
        ierr = nf90_inq_varid(ncid(0),datatype,varid(0))    
      else
        ncfile(0) = .false.
        ! Must be compiled using 1 byte record lengths
        Open(40,FILE=datafilename,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=10800,STATUS='OLD')
      end if
    end if 
  case('change')
    write(6,*) 'Process land-use change dataset.'
    scalelimit=300
    ierr = nf90_open(datafilename,nf90_nowrite,ncid(0))
    if ( ierr==nf90_noerr ) then
      write(6,*) "Found netcdf version ",trim(datafilename)
    else  
      write(6,*) "ERROR: Cannot open ",trim(datafilename)
      call finishbanner
      stop -1
    end if
  Case DEFAULT
    Write(6,*) 'ERROR: Cannot find data ',trim(datatype)
    call finishbanner
    Stop -1
End Select

If (fastigbp) then

  ! Step over scales
  mode=0
  Do While (Any(countn==0).AND.nscale>scalelimit)

    latlon=(/ baselon, 90. /)
    ltest=countn==0
    Call findsmallscale(nscale,scalelimit,latlon,llstore,grid,ltest,rlld,subsec,sll,sibsize,sibdim)

    slonn=sll(1,1)
    slatx=sll(2,2)

    minscale=nscale*binlimit

    Write(6,*) 'Bin'
    Write(6,*) 'nscale       = ',nscale
    Write(6,*) 'subsec       = ',subsec
    Write(6,*) 'sll          = ',sll
    Write(6,*) 'llstore      = ',llstore

    If (subsec/=0) then

      Do nx=1,subsec
        Do ny=1,subsec

          Write(6,*) 'nx,ny,subsec = ',nx,ny,subsec
      
          lldim=llstore
          ! Determine top corner lat/lon
          Call latlonconvert(nscale,latlon,lldim,slonn,slatx,nx,ny)
      
          Write(6,*) 'orig latlon  = ',latlon
          Write(6,*) 'orig lldim   = ',lldim

          ! Check if there are any points of interest on this tile
          ltest=countn==0
          Call searchdim(mode,sll,nscale,real(nscale),latlon,lldim,grid,ltest,rlld,sibdim)
          Call scaleconvert(nscale,tmp,lldim,sll,sibsize)
          mode=2
      
          latlon(1)=sll(1,1)
          latlon(2)=sll(2,2)

          Write(6,*) 'mod latlon   = ',latlon
          Write(6,*) 'mod lldim    = ',lldim

          ! Bin
          If (All(lldim>0)) then

            Allocate(coverout(lldim(1),lldim(2),0:num))
            allocate( lcmap(lldim(1),lldim(2),2) )
  
            Select Case(datatype)
              Case('land')
                Call kmconvert(nscale,nscale_x,lldim,lldim_x,10)
                Call igbpread(latlon,nscale_x,lldim_x,coverout,num,month,datafilename,laifilename,class_num,mapjveg, &
                              ncid,varid,ncfile)
              Case('land2')
                Call kmconvert(nscale,nscale_x,lldim,lldim_x,5)
                Call modisread(latlon,nscale_x,lldim_x,coverout,num,month,datafilename,laifilename,class_num,mapjveg, &
                               ncid,varid,ncfile)
              Case('soil')
                Call kmconvert(nscale,nscale_x,lldim,lldim_x,40)
                Call soilread(latlon,nscale_x,lldim_x,coverout,datafilename,ncid(0),varid(0),ncfile(0))
              Case('albvis','albnir')
                Call kmconvert(nscale,nscale_x,lldim,lldim_x,40)
                Call albedoread(latlon,nscale_x,lldim_x,coverout(:,:,0),datatype,datafilename,ncid(0),varid(0),ncfile(0))
              case('change')
                call kmconvert(nscale,nscale_x,lldim,lldim_x,300)
                call changeread(latlon,nscale_x,lldim_x,coverout,datafilename,year,ncid(0))    
              Case DEFAULT
                Write(6,*) 'ERROR: Cannot find data ',trim(datatype)
                call finishbanner
                Stop -1
            End Select

            write(6,*) 'Start bin'
            ltest = grid*10.>=real(minscale)
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) SHARED(lldim,latlon,nscale,sibdim,lcmap) &
!$OMP   PRIVATE(j,aglat,i,aglon,alci,alcj,nface,lci,lcj)
            do j = 1,lldim(2)
              aglat = callat(latlon(2),j,nscale)
              do i = 1,lldim(1)           
                aglon = callon(latlon(1),i,nscale)
                call lltoijmod(aglon,aglat,alci,alcj,nface)
                lci = nint(alci)
                lcj = nint(alcj)
                lcj = lcj + nface*sibdim(1)
                lcmap(i,j,1) = lci
                lcmap(i,j,2) = lcj
              end do
            end do  
!$OMP END PARALLEL DO
            
            if ( datatype=='land' .or. datatype=='land2' ) then

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(jj,j,i,k,lci,lcj,newcover,newdata)
              do jj = 1,sibdim(2)
                do j = 1,lldim(2)
                  do i = 1,lldim(1)           
                    lci = lcmap(i,j,1) 
                    lcj = lcmap(i,j,2)
                    if ( lcj==jj .and. ltest(lci,lcj) ) then
                      newcover(0:num) = coverout(i,j,0:num)
                      newdata(0:num) = dataout(lci,lcj,0:num)
                      if ( sum(abs(newcover(0:class_num)))<0.001 ) then
                        if ( countn(lci,lcj)==0 ) then
                          dataout(lci,lcj,0:num) = -1. ! Missing value?
                          countn(lci,lcj) = 1
                        end if
                      else
                        if ( newdata(0)<0. ) then
                          newdata(0:num) = 0. ! reset missing point after finding non-trival data
                          countn(lci,lcj) = 0
                        end if
                        newdata(0:class_num) = newdata(0:class_num) + newcover(0:class_num)
                        do k = class_num+1,num
                          if ( newcover(k)==0. .and. countn(lci,lcj)>0 ) then
                            newdata(k) = newdata(k)*real(countn(lci,lcj)+1) &
                                /real(countn(lci,lcj))
                          else if ( newdata(k)==0. ) then
                            newdata(k) = newcover(k)*real(countn(lci,lcj)+1)
                          else
                            newdata(k) = newdata(k) + newcover(k)
                          end if
                        end do  
                        dataout(lci,lcj,0:num) = newdata(0:num)
                        countn(lci,lcj) = countn(lci,lcj) + 1
                      end if
                    end if
                  end do
                end do
              end do
!$OMP END PARALLEL DO
            else
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(jj,j,i,lci,lcj)
              do jj = 1,sibdim(2)  
                do j = 1,lldim(2)
                  do i = 1,lldim(1)              
                    lci = lcmap(i,j,1)
                    lcj = lcmap(i,j,2)
                    if ( lcj==jj .and. ltest(lci,lcj) ) then
                      if ( sum(abs(coverout(i,j,:)))<=0.01 ) then
                        if ( countn(lci,lcj)==0 ) then
                          dataout(lci,lcj,:) = -1. ! Missing value?
                          countn(lci,lcj) = 1
                        end if
                      else
                        if ( dataout(lci,lcj,0)<0. ) then
                          dataout(lci,lcj,:) = 0. ! reset missing point after finding non-trival data
                          countn(lci,lcj) = 0
                        end If
                        dataout(lci,lcj,:) = dataout(lci,lcj,:) + coverout(i,j,:)
                        countn(lci,lcj) = countn(lci,lcj) + 1                      
                      end if
                    end if
                  end do  
                end do
              end do
!$OMP END PARALLEL DO
            end if
            write(6,*) 'Bin complete'

            deallocate( coverout, lcmap )

          else
            Write(6,*) 'No points in valid range'
          end If
      
        end Do
      end Do

    else
      write(6,*) 'Skip'
    end If
  
  end Do

Else

  Select Case(datatype)
    Case('land')
      Call igbpstream(sibdim,dataout,countn,num,month,datafilename,laifilename,class_num,mapjveg, &
                      ncid,varid,ncfile)
    Case('land2')
      Call modisstream(sibdim,dataout,countn,num,month,datafilename,laifilename,class_num,mapjveg, &
                       ncid,varid,ncfile)
    Case('soil')
      Call soilstream(sibdim,dataout,countn,datafilename,ncid(0),varid(0),ncfile(0))
    Case('albvis','albnir')
      Call albedostream(sibdim,dataout(:,:,0),countn,datatype,datafilename,ncid(0),varid(0),ncfile(0))
    case('change')
      call changestream(sibdim,dataout,countn,datafilename,year,ncid(0))
    Case DEFAULT
      Write(6,*) 'ERROR: Cannot find data ',trim(datatype)
      call finishbanner
      Stop -1
  End Select

End If

! Fill
Write(6,*) 'Fill'
Allocate(sermask(1:2,1:2))
nscale=scalelimit

latlon=(/ baselon, 90. /)
llstore=(/ 432000/nscale , 216000/nscale /)
ltest=countn==0
Call searchdim(4,sll,nscale,0.,latlon,llstore,grid,ltest,rlld,sibdim)
Call scaleconvert(nscale,subsec,llstore,sll,sibsize)
slonn=sll(1,1)
slatx=sll(2,2)

Write(6,*) 'nscale       = ',nscale
Write(6,*) 'subsec       = ',subsec
Write(6,*) 'sll          = ',sll
Write(6,*) 'llstore      = ',llstore

If (subsec/=0) then
  Do nx=1,subsec
    Do ny=1,subsec

      Write(6,*) 'nx,ny,subsec = ',nx,ny,subsec

      lldim=llstore
      Call latlonconvert(nscale,latlon,lldim,slonn,slatx,nx,ny)

      Write(6,*) 'orig latlon  = ',latlon
      Write(6,*) 'orig lldim   = ',lldim

      ! overlap tiles for interpolation
      If (nx/=subsec) lldim(1)=lldim(1)+1
      If (ny/=subsec) lldim(2)=lldim(2)+1
    
      ! Check if there are any points of interest on this tile
      ltest=countn==0
      Call searchdim(4,sll,nscale,0.,latlon,lldim,grid,ltest,rlld,sibdim)
      Call scaleconvert(nscale,tmp,lldim,sll,sibsize)
      If (Any(lldim(:)==1)) lldim=0
      
      latlon(1)=sll(1,1)
      latlon(2)=sll(2,2)

      Write(6,*) 'mod latlon   = ',latlon
      Write(6,*) 'mod lldim    = ',lldim

      If (lldim(1)>0.AND.lldim(2)>0) then

        Allocate(coverout(lldim(1),lldim(2),0:num))

        Select Case(datatype)
          Case('land')
            Call kmconvert(nscale,nscale_x,lldim,lldim_x,10)
            Call igbpread(latlon,nscale_x,lldim_x,coverout,num,month,datafilename,laifilename,class_num,mapjveg, &
                          ncid,varid,ncfile)
          Case('land2')
            Call kmconvert(nscale,nscale_x,lldim,lldim_x,5)
            Call modisread(latlon,nscale_x,lldim_x,coverout,num,month,datafilename,laifilename,class_num,mapjveg, &
                           ncid,varid,ncfile)
          Case('soil')
            Call kmconvert(nscale,nscale_x,lldim,lldim_x,40)
            Call soilread(latlon,nscale_x,lldim_x,coverout,datafilename,ncid(0),varid(0),ncfile(0))
          Case('albvis','albnir')
            Call kmconvert(nscale,nscale_x,lldim,lldim_x,40)
            Call albedoread(latlon,nscale_x,lldim_x,coverout(:,:,0),datatype,datafilename,ncid(0),varid(0),ncfile(0))
          case('change')
            call kmconvert(nscale,nscale_x,lldim,lldim_x,300)
            call changeread(latlon,nscale_x,lldim_x,coverout,datafilename,year,ncid(0))
          Case DEFAULT
            Write(6,*) 'ERROR: Cannot find data ',trim(datatype)
            call finishbanner
            Stop -1
        End Select

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) SHARED(sibdim,countn,rlld,latlon,nscale,lldim,coverout,dataout) &
!$OMP   PRIVATE(lcj,lci,aglon,aglat,serlon,serlat,i,j)          
        Do lcj=1,sibdim(2)
          Do lci=1,sibdim(1)        
            If (countn(lci,lcj)==0) then
              aglon=rlld(lci,lcj,1)
              aglat=rlld(lci,lcj,2)
              serlon=indexlon(aglon,latlon(1),nscale)
              serlat=indexlat(aglat,latlon(2),nscale)
              i=nint(serlon)
              j=nint(serlat)
              if (i>0.and.i<=lldim(1).and.j>0.and.j<=lldim(2)) then
                if (any(coverout(i,j,:)>0.)) then
                  dataout(lci,lcj,:)=coverout(i,j,:)
                  countn(lci,lcj)=1
                else
                  ! missing
                  dataout(lci,lcj,:)=-1.
                  countn(lci,lcj)=1
                end if
              end if
            End If
          End Do
        End Do
!$OMP END PARALLEL DO        
        
        Deallocate(coverout)

      Else
        Write(6,*) 'No points in valid range'
      End If
    End Do
  End Do

Else
  Write(6,*) 'Skip'
End If

Deallocate(sermask)

! clear missing values (e.g., ocean points)
! These values should be fixed in igbpveg.f90
countn=max(countn,1)
dataout=max(dataout,0.)

Do k=0,num
  dataout(:,:,k)=dataout(:,:,k)/Real(countn)
End Do

if (datatype=='land' .or. datatype=='land2') then
  Allocate(sermask0(1:sibdim(1),1:sibdim(2),1:class_num),sermask2(1:sibdim(1),1:sibdim(2),1:class_num))
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(mapwater,dataout,mthrng,class_num,sibdim,rlld,sermask0,sermask2) &
!$OMP   PRIVATE(k,lci,lcj,i,pxy,netlai,netcount)
  do k=1,class_num
    if ( .not.mapwater(k) ) then
      sermask0(:,:,k)=dataout(:,:,k)>0.
      sermask2(:,:,k)=sermask0(:,:,k)
      where ( sermask0(:,:,k) .and. any(dataout(:,:,(k-1)*mthrng+class_num+1:k*mthrng+class_num)==0.,dim=3) )
        sermask2(:,:,k) = .false.
      elsewhere ( sermask0(:,:,k) )
        sermask0(:,:,k) = .false.
      end where
      if (any(sermask0(:,:,k))) then ! missing LAI data
        Write(6,*) "Replace missing LAI for class ",k
        if (any(sermask2(:,:,k))) then
          call fill_cc_a_mask(dataout(:,:,(k-1)*mthrng+class_num+1:k*mthrng+class_num),sibdim(1),mthrng, &
                              sermask2(:,:,k),sermask0(:,:,k))
        else
          sermask2(:,:,k)=.false.
          do lcj=1,sibdim(2)
            do lci=1,sibdim(1)
              i=1
              do while(i<=class_num.and..not.sermask2(lci,lcj,k))
                if ( .not.mapwater(i) ) then
                  if (all(dataout(lci,lcj,(i-1)*mthrng+class_num+1:i*mthrng+class_num)>0.)) then
                    sermask2(lci,lcj,k)=.true.
                  end if
                end if
                i=i+1
              end do
            end do
          end do
          if (any(sermask2(:,:,k))) then
            write(6,*) "Extended replace for missing LAI class ",k
            do lcj=1,sibdim(2)
              do lci=1,sibdim(1)
                if (sermask0(lci,lcj,k)) then
                  call findnear(pxy,lci,lcj,sermask2(:,:,k),rlld,sibdim)
                  netlai=0.
                  netcount=0
                  do i=1,class_num
                    if ( .not.mapwater(i) ) then
                      if (all(dataout(pxy(1),pxy(2),(i-1)*mthrng+class_num+1:i*mthrng+class_num)>0.)) then
                        netlai(1:mthrng)=netlai(1:mthrng)+dataout(pxy(1),pxy(2),(i-1)*mthrng+class_num+1:i*mthrng+class_num)
                        netcount=netcount+1
                      end if
                    end if
                  end do
                  dataout(lci,lcj,(k-1)*mthrng+class_num+1:k*mthrng+class_num)=netlai(1:mthrng)/real(netcount)
                end if
              end do
            end do
          else
            write(6,*) "Only trivial LAI found"
          end if
        end if
      end if
    else
      dataout(:,:,class_num+(k-1)*mthrng+1:class_num+k*mthrng)=0.  
    end if
  end do
!$OMP END PARALLEL DO
  Deallocate(sermask0,sermask2)
end if

! close files
Select Case(datatype)
  Case('land','land2')
    if ( ncfile(0) ) then
      ierr = nf90_close(ncid(0))
    else  
      Close(10)
    end if  
    do imth=1,mthrng
      if ( ncfile(imth) ) then
        ierr = nf90_close(ncid(imth))
      else  
        close(10+imth)
      end if
    end do
  Case('soil')
    if ( ncfile(0) ) then
      ierr = nf90_close(ncid(0))
    else  
      Close(20)
    end if  
  Case('albvis','albnir')
    if ( ncfile(0) ) then
      ierr = nf90_close(ncid(0))  
    else
      Close(40)
    end if
  case('change')
    ierr = nf90_close(ncid(0))
end select      
      
Write(6,*) "Task complete"

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads sib data down to nscale=1km resolution
!

Subroutine igbpread(latlon,nscale,lldim,coverout,num,month,vegfilename,laifilename,class_num,mapjveg, &
                    ncid,varid,ncfile)

use netcdf_m

Implicit None

Integer, intent(in) :: nscale,num,month,class_num
Real, dimension(1:2), intent(in) :: latlon
Integer, dimension(1:2), intent(in) :: lldim
Real, dimension(lldim(1),lldim(2),0:num), intent(inout) :: coverout
Integer*1, dimension(43200) :: datatemp
integer, dimension(43200,nscale) :: databuffer
Integer, dimension(43200) :: ltemp2
integer, dimension(:,:,:), allocatable :: lbuff
integer, dimension(43200) :: i4datatemp
integer, dimension(0:num) :: ncount
Integer, dimension(2,2) :: jin,jout
integer, dimension(class_num), intent(in) :: mapjveg
Integer ilat,ilon,jlat,recpos,mthrng,imth,lrp,ctmp,ltmp,nlrp,k
integer i,j,ntmp,ix,iy,tiy,tix,vegtmp
Integer llint1, llint2
integer ierr
integer, dimension(0:12), intent(in) :: ncid, varid
logical, dimension(0:12), intent(in) :: ncfile
real bx,by,bdelta,tbx,tby,tbdelta
character(len=2) cmth
Character(len=10) fname
character(len=*), intent(in) :: vegfilename, laifilename

if ( month==0 ) then
  mthrng=12
else
  mthrng=1
end if

allocate(lbuff(43200,nscale,mthrng))

! To speed-up the code, 43200x(nscale) blocks of the igbp file are read
! at a time.  The data is then averaged in memory.  This system speeds-up the
! code considerably.  However, there are limitations as to the size of data
! that can be stored in memory.

Call solvejshift(latlon(1),jin,jout,120)

lrp=-1
coverout=0.

Do ilat=1,lldim(2)

  if ((mod(ilat,50).eq.0).or.(ilat.eq.lldim(2))) then
    Write(6,*) 'USGS + LAI - ',ilat,'/',lldim(2)
  end if
  
  ! Read data
  llint2=nint((90.-latlon(2))*120.)+(ilat-1)*nscale
  Do jlat=1,nscale
    recpos=llint2+jlat
    if ( ncfile(0) ) then
      ierr = nf90_get_var(ncid(0),varid(0),i4datatemp,start=(/1,recpos/),count=(/43200,1/))  
    else  
      Read(10,REC=recpos) datatemp
      i4datatemp = datatemp
    end if  
    ! Shift lon to zero
    databuffer(jin(1,1):jin(1,2),jlat)=i4datatemp(jout(1,1):jout(1,2))
    databuffer(jin(2,1):jin(2,2),jlat)=i4datatemp(jout(2,1):jout(2,2))
  End Do
  
  do imth=1,mthrng
    lrp=-1
    Do jlat=1,nscale
      recpos=llint2+jlat
      ! read corrosponding lai data and fill to 1km grid
      nlrp=int(real(recpos+3)/4.)
      if (lrp/=nlrp) then
        lrp=nlrp
        if ( ncfile(imth) ) then
          ierr = nf90_get_var(ncid(imth),varid(imth),i4datatemp(1:10800), &
                   start=(/1,lrp/),count=(/10800,1/))  
        else  
          read(10+imth,REC=lrp) datatemp(1:10800)
          i4datatemp(1:10800) = datatemp(1:10800)
        end if  
        do k=1,10800
          ltemp2(4*k-3:4*k)=i4datatemp(k)
        end do
      end if
      lbuff(jin(1,1):jin(1,2),jlat,imth)=ltemp2(jout(1,1):jout(1,2))
      lbuff(jin(2,1):jin(2,2),jlat,imth)=ltemp2(jout(2,1):jout(2,2))
    End Do
  end do
 
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(ilon,llint1,ncount,j,i,vegtmp,ctmp) &
!$OMP   PRIVATE(k,imth,ltmp)
  Do ilon=1,lldim(1)
    llint1=(ilon-1)*nscale
    ncount(:)=0
    do j=1,nscale
      do i=llint1+1,llint1+nscale
        vegtmp=mod(databuffer(i,j)+256,256)
        if ( vegtmp==0 ) then
          ctmp=0
        else if ( mapjveg(max(min(vegtmp,class_num),1))==vegtmp ) then
          ctmp=max(min(vegtmp,class_num),1)
        else
          ctmp = -1
          do k = 1,class_num
            if ( vegtmp==mapjveg(k) ) then
              ctmp = k
              exit
            end if
          end do
        end if
        if (ctmp>=0.and.ctmp<=class_num) then
          coverout(ilon,ilat,ctmp)=coverout(ilon,ilat,ctmp)+1.
          ncount(ctmp)=ncount(ctmp)+1
          if ( ctmp>0 ) then
            do imth=1,mthrng
              ltmp=mod(lbuff(i,j,imth)+256,256)
              if (ltmp>0.and.ltmp<100) then
                coverout(ilon,ilat,class_num+(ctmp-1)*mthrng+imth)=coverout(ilon,ilat,class_num+(ctmp-1)*mthrng+imth)+real(ltmp)/10.
                ncount(class_num+(ctmp-1)*mthrng+imth)=ncount(class_num+(ctmp-1)*mthrng+imth)+1
              end if
            end do
          end if
        end if
      end do
    end do
    ncount(0:class_num)=sum(ncount(0:class_num))
    coverout(ilon,ilat,:)=coverout(ilon,ilat,:)/real(max(ncount,1))
  End Do
!$OMP END PARALLEL DO  
 
End Do

deallocate(lbuff)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads modis data down to nscale=1km resolution    
    
Subroutine modisread(latlon,nscale,lldim,coverout,num,month,vegfilename,laifilename,class_num,mapjveg, &
                     ncid,varid,ncfile)

use netcdf_m

Implicit None

Integer, intent(in) :: nscale,num,month,class_num
Real, dimension(1:2), intent(in) :: latlon
Integer, dimension(1:2), intent(in) :: lldim
Real, dimension(lldim(1),lldim(2),0:num), intent(inout) :: coverout
Integer*1, dimension(86400) :: datatemp
integer, dimension(86400,nscale) :: databuffer
Integer, dimension(86400) :: ltemp2
integer, dimension(:,:,:), allocatable :: lbuff
integer, dimension(86400) :: i4datatemp
integer, dimension(0:num) :: ncount
Integer, dimension(2,2) :: jin,jout
integer, dimension(class_num), intent(in) :: mapjveg
Integer ilat,ilon,jlat,recpos,mthrng,imth,lrp,ctmp,ltmp,nlrp,k
integer i,j,ntmp,ix,iy,tiy,tix,vegtmp
Integer llint1, llint2
integer ierr
integer, dimension(0:12), intent(in) :: ncid, varid
logical, dimension(0:12), intent(in) :: ncfile
real, dimension(:,:,:), allocatable :: laiin
real bx,by,bdelta,tbx,tby,tbdelta
character(len=2) cmth
Character(len=10) fname
character(len=*), intent(in) :: vegfilename, laifilename

if ( month==0 ) then
  mthrng=12
else
  mthrng=1
end if

allocate(lbuff(86400,nscale,mthrng))

! To speed-up the code, 86400x(nscale) blocks of the modis file are read
! at a time.  The data is then averaged in memory.  This system speeds-up the
! code considerably.  However, there are limitations as to the size of data
! that can be stored in memory.

Call solvejshift(latlon(1),jin,jout,240)

lrp=-1
coverout=0.

Do ilat=1,lldim(2)

  if ((mod(ilat,50).eq.0).or.(ilat.eq.lldim(2))) then
    Write(6,*) 'MODIS + LAI - ',ilat,'/',lldim(2)
  end if
  
  ! Read data
  llint2=nint((90.-latlon(2))*240.)+(ilat-1)*nscale
  Do jlat=1,nscale
    recpos=llint2+jlat
    ierr = nf90_get_var(ncid(0),varid(0),i4datatemp,start=(/1,recpos/),count=(/86400,1/))  
    ! Shift lon to zero
    databuffer(jin(1,1):jin(1,2),jlat)=i4datatemp(jout(1,1):jout(1,2))
    databuffer(jin(2,1):jin(2,2),jlat)=i4datatemp(jout(2,1):jout(2,2))
  End Do
  
  do imth=1,mthrng
    lrp=-1
    Do jlat=1,nscale
      recpos=llint2+jlat
      ! read corrosponding lai data and fill to 1km grid
      nlrp=int(real(recpos+7)/8.)
      if (lrp/=nlrp) then
        lrp=nlrp
        if ( ncfile(imth) ) then
          ierr = nf90_get_var(ncid(imth),varid(imth),i4datatemp(1:10800), &
                   start=(/1,lrp/),count=(/10800,1/))  
        else  
          read(10+imth,REC=lrp) datatemp(1:10800)
          i4datatemp(1:10800) = datatemp(1:10800)
        end if  
        do k=1,10800
          ltemp2(8*k-7:8*k)=i4datatemp(k)
        end do
      end if
      lbuff(jin(1,1):jin(1,2),jlat,imth)=ltemp2(jout(1,1):jout(1,2))
      lbuff(jin(2,1):jin(2,2),jlat,imth)=ltemp2(jout(2,1):jout(2,2))
    End Do
  end do

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(ilon,llint1,ncount,j,i,vegtmp) &
!$OMP   PRIVATE(ctmp,k,imth,ltmp)
  Do ilon=1,lldim(1)
    llint1=(ilon-1)*nscale
    ncount=0
    do j=1,nscale
      do i=llint1+1,llint1+nscale
        vegtmp=mod(databuffer(i,j)+256,256)
        if ( vegtmp==0 ) then
          ctmp=0
        else if ( mapjveg(max(min(vegtmp,class_num),1))==vegtmp ) then
          ctmp=max(min(vegtmp,class_num),1)
        else
          ctmp = -1
          do k = 1,class_num
            if ( vegtmp==mapjveg(k) ) then
              ctmp = k
              exit
            end if
          end do
        end if
        if (ctmp>=0.and.ctmp<=class_num) then
          coverout(ilon,ilat,ctmp)=coverout(ilon,ilat,ctmp)+1.
          ncount(ctmp)=ncount(ctmp)+1
          if ( ctmp>0 ) then
            do imth=1,mthrng
              ltmp=mod(lbuff(i,j,imth)+256,256)
              if (ltmp>0.and.ltmp<100) then
                coverout(ilon,ilat,class_num+(ctmp-1)*mthrng+imth)=coverout(ilon,ilat,class_num+(ctmp-1)*mthrng+imth)+real(ltmp)/10.
                ncount(class_num+(ctmp-1)*mthrng+imth)=ncount(class_num+(ctmp-1)*mthrng+imth)+1
              end if
            end do
          end if
        end if
      end do
    end do
    ncount(0:class_num)=sum(ncount(0:class_num))
    coverout(ilon,ilat,:)=coverout(ilon,ilat,:)/real(max(ncount,1))
  End Do
!$OMP END PARALLEL DO  
 
End Do

deallocate(lbuff)

Return
End
    
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads soil data down to nscale_4=1
! (i.e., 4km) resolution.
!

Subroutine soilread(latlon,nscale_4,lldim_4,coverout,soildatafile,ncid,varid,ncfile)

use netcdf_m

Implicit None

Integer, intent(in) :: nscale_4
Real, dimension(2), intent(in) :: latlon
Integer, dimension(2), intent(in) :: lldim_4
Real, dimension(lldim_4(1),lldim_4(2),0:8), intent(out) :: coverout
real, dimension(0:13) :: faosoil
integer, dimension(nscale_4,nscale_4) :: dataslice
Integer, dimension(10800,1:nscale_4) :: databuffer
Integer*1, dimension(10800) :: datatemp
integer, dimension(10800) :: i4datatemp
Integer, dimension(2,2) :: jin,jout
Integer ilat,ilon,jlat,recpos,i
integer ierr
integer, intent(in) :: ncid, varid
Integer llint_41, llint_42
logical, intent(in) :: ncfile
real nsum
integer, dimension(0:13), parameter :: masmap=(/ 0, 1, 1, 4, 2, 4, 7, 2, 2, 5, 6, 3, 8, 9 /)
character(len=*), intent(in) :: soildatafile

! To speed-up the code, 43200x(nscale) blocks of the sib file are read
! at a time.  The data is then averaged in memory.  This system speeds-up the
! code considerably.  However, there are limitations as to the size of data
! that can be stored in memory.

Call solvejshift(latlon(1),jin,jout,30)

Do ilat=1,lldim_4(2)

  if ((mod(ilat,50).eq.0).or.(ilat.eq.lldim_4(2))) then
    Write(6,*) 'HWSD - ',ilat,'/',lldim_4(2)
  end if
  
  ! Read data
  llint_42=nint((90.-latlon(2))*30.)+(ilat-1)*nscale_4
  Do jlat=1,nscale_4
    recpos=llint_42+jlat
    if ( ncfile ) then
      ierr = nf90_get_var(ncid,varid,i4datatemp,start=(/1,recpos/),count=(/10800,1/))  
    else    
      Read(20,REC=recpos) datatemp
      i4datatemp = datatemp
    end if  
    ! Shift lon to zero
    databuffer(jin(1,1):jin(1,2),jlat)=i4datatemp(jout(1,1):jout(1,2))
    databuffer(jin(2,1):jin(2,2),jlat)=i4datatemp(jout(2,1):jout(2,2))
  End Do
  
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(ilon,llint_41,dataslice,nsum,i)  
  Do ilon=1,lldim_4(1)
    llint_41=(ilon-1)*nscale_4
    dataslice(1:nscale_4,1:nscale_4) = databuffer(llint_41+1:llint_41+nscale_4,1:nscale_4)
    Call dataconvert(dataslice(:,:),faosoil,nscale_4,13)
    nsum=sum(faosoil(1:13))
    if (nsum>0.) then
      coverout(ilon,ilat,:)=0.
      do i=1,13
        coverout(ilon,ilat,masmap(i)-1)=coverout(ilon,ilat,masmap(i)-1)+faosoil(i)/nsum
      end do
    else
      coverout(ilon,ilat,:)=0.
    end if
  End Do
!$OMP END PARALLEL DO  
  
End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads albedo data down to nscale_4=1
! (i.e., 4km) resolution.
!

Subroutine albedoread(latlon,nscale_4,lldim_4,dataout,datatype,albfilename,ncid,varid,ncfile)

use netcdf_m

Implicit None

Integer, intent(in) :: nscale_4
Integer, dimension(2), intent(in) :: lldim_4
Integer, dimension(10800,1:nscale_4) :: databuffer
Integer*1, dimension(10800) :: datatemp
integer, dimension(10800) :: i4datatemp
Integer llint_41, llint_42
Integer ilat,ilon,jlat,recpos,ncount
Integer, dimension(2,2) :: jin,jout
integer ierr
integer, intent(in) :: ncid, varid
Real, dimension(2), intent(in) :: latlon
Real, dimension(lldim_4(1),lldim_4(2)), intent(out) :: dataout
Character(len=*), intent(in) :: datatype
character(len=*), intent(in) :: albfilename
Character(len=20) :: cmsg
Logical, dimension(nscale_4,nscale_4) :: sermask
logical, intent(in) :: ncfile

Call solvejshift(latlon(1),jin,jout,30)

select case(datatype)
  case ('albvis')
    cmsg='Soil albedo (VIS) - '
  case ('albnir')
    cmsg='Soil albedo (NIR) - '
end select

Do ilat=1,lldim_4(2)

  if ( mod(ilat,50)==0 .or. ilat==lldim_4(2) ) then
    Write(6,*) cmsg,ilat,'/',lldim_4(2)
  end if
  
  ! Read data
  llint_42=nint((90.-latlon(2))*30.)+(ilat-1)*nscale_4
  Do jlat=1,nscale_4
    recpos=llint_42+jlat
    if ( ncfile ) then
      ierr = nf90_get_var(ncid,varid,i4datatemp,start=(/1,recpos/),count=(/10800,1/))  
    else  
      Read(40,REC=recpos) datatemp
      i4datatemp = datatemp
    end if  
    ! Shift lon to zero
    databuffer(jin(1,1):jin(1,2),jlat)=i4datatemp(jout(1,1):jout(1,2))
    databuffer(jin(2,1):jin(2,2),jlat)=i4datatemp(jout(2,1):jout(2,2))
  End Do
  
  where (databuffer<0)
    databuffer=databuffer+256
  end where
  
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(ilon,llint_41,sermask,ncount)  
  Do ilon=1,lldim_4(1)
    llint_41=(ilon-1)*nscale_4
    sermask=(databuffer(llint_41+1:llint_41+nscale_4,1:nscale_4) > 0)
    sermask=sermask.and.(databuffer(llint_41+1:llint_41+nscale_4,1:nscale_4) <= 100)
    ncount = count(sermask)
    if ( ncount > 0 ) then
      dataout(ilon,ilat)=real(sum(databuffer(llint_41+1:llint_41+nscale_4,1:nscale_4),sermask)) &
                        /(real(ncount)*100.)
    else
      dataout(ilon,ilat)=0. ! missing value flag
    end if
  End Do
!$OMP END PARALLEL DO

End Do

Return
End

    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads land-use data down to nscale_30=1
! (i.e., 30km) resolution.
!

Subroutine changeread(latlon,nscale_30,lldim_30,coverout,changedatafile,year,ncid)

use netcdf_m

Implicit None

Integer, intent(in) :: nscale_30, year
Real, dimension(2), intent(in) :: latlon
Integer, dimension(2), intent(in) :: lldim_30
Real, dimension(lldim_30(1),lldim_30(2),0:2), intent(out) :: coverout
Integer, dimension(1440,1:nscale_30) :: databuffer
real, dimension(1440) :: datatemp
Integer, dimension(2,2) :: jin,jout
Integer ilat,ilon,jlat,recpos,i
integer varid, ierr, indx, n, dimid
integer iposa, iposb, rectime, yyyy, dimlen
integer, intent(in) :: ncid
Integer llint_301, llint_302
real nsum
Logical, dimension(nscale_30,nscale_30) :: sermask
character(len=*), intent(in) :: changedatafile
character(len=32) units

coverout = 0.

ierr = nf90_inq_varid(ncid,"time",varid)
ierr = nf90_get_att(ncid,varid,"units",units)
iposa = index(trim(units),'since')
iposa = iposa + 5 ! skip 'since'
iposb = index(trim(units(iposa:)),'-')
iposb = iposa + iposb - 2 ! remove '-'
read(units(iposa:iposb),FMT=*,iostat=ierr) yyyy
if ( ierr/=0 ) then
  write(6,*) "ERROR: Cannot read time units. Expecting year but found ",units(iposa:iposb)
  call finishbanner
  stop -1
end if
rectime = year - yyyy + 1
ierr = nf90_inq_dimid(ncid,'time',dimid)
ierr = nf90_inquire_dimension(ncid,dimid,len=dimlen)
if ( rectime>dimlen .or. rectime<1 ) then
  write(6,*) "ERROR: year not found in input file"
  write(6,*) "year = ",year
  write(6,*) "file = ", trim(changedatafile)
  call finishbanner
  stop -1
end if

! To speed-up the code, 43200x(nscale) blocks of the sib file are read
! at a time.  The data is then averaged in memory.  This system speeds-up the
! code considerably.  However, there are limitations as to the size of data
! that can be stored in memory.

Call solvejshift(latlon(1),jin,jout,4)

Do ilat=1,lldim_30(2)

  if ((mod(ilat,50).eq.0).or.(ilat.eq.lldim_30(2))) then
    Write(6,*) 'Change - ',ilat,'/',lldim_30(2)
  end if

  do n = 1,7
    
    select case(n)
      case(1)
        ierr = nf90_inq_varid(ncid,"c3ann",varid)
        indx = 0 ! C3 crop
      case(2)
        ierr = nf90_inq_varid(ncid,"c4ann",varid)
        indx = 1 ! C4 crop
      case(3)
        ierr = nf90_inq_varid(ncid,"c3per",varid)
        indx = 0 ! C3 crop
      case(4)
        ierr = nf90_inq_varid(ncid,"c4per",varid)
        indx = 1 ! C4 crop
      case(5)
        ierr = nf90_inq_varid(ncid,"c3nfx",varid)
        indx = 0 ! C3 crop
      case(6)
        ierr = nf90_inq_varid(ncid,"pastr",varid)
        indx = 2 ! Pasture
      case(7)
        ierr = nf90_inq_varid(ncid,"range",varid)
        indx = 2 ! Pasture
    end select    

    ! Read data
    llint_302=nint((90.-latlon(2))*4.)+(ilat-1)*nscale_30
    Do jlat=1,nscale_30
      recpos=llint_302+jlat
      ierr = nf90_get_var(ncid,varid,datatemp,start=(/1,recpos,rectime/),count=(/1440,1,1/))
      ! Shift lon to zero
      databuffer(jin(1,1):jin(1,2),jlat)=datatemp(jout(1,1):jout(1,2))
      databuffer(jin(2,1):jin(2,2),jlat)=datatemp(jout(2,1):jout(2,2))
    End Do

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(ilon,llint_301,sermask,nsum)    
    Do ilon=1,lldim_30(1)
      llint_301=(ilon-1)*nscale_30
      sermask = databuffer(llint_301+1:llint_301+nscale_30,1:nscale_30)>=0. .and. &
                databuffer(llint_301+1:llint_301+nscale_30,1:nscale_30)<=1.
      nsum=count(sermask)
      coverout(ilon,ilat,indx)=coverout(ilon,ilat,indx) &
          +sum(databuffer(llint_301+1:llint_301+nscale_30,1:nscale_30),mask=sermask)/real(nsum)  
    End Do
!$OMP END PARALLEL DO    
    
  end do  
    
End Do

Return
End
        
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads sibsystems data at nscale=1km resolution
! (i.e., no storage, simply read and bin)
!

Subroutine igbpstream(sibdim,coverout,countn,num,month,vegfilename,laifilename,class_num,mapjveg, &
                      ncid,varid,ncfile)

Use ccinterp
use netcdf_m

Implicit None

integer, intent(in) :: num,month,class_num
Integer, dimension(2), intent(in) :: sibdim
Real, dimension(1:sibdim(1),1:sibdim(2),0:num), intent(out) :: coverout
Real aglon,aglat,alci,alcj,bx,by,bdelta,tbx,tby,tbdelta
Real callon,callat
Integer, dimension(1:sibdim(1),1:sibdim(2)), intent(out) :: countn
integer, dimension(class_num), intent(in) :: mapjveg
Integer*1, dimension(1:43200) :: databuffer
Integer*1, dimension(1:10800) :: datatemp
Integer*1, dimension(1:43200,1:12) :: lbuff
integer, dimension(43200) :: i4databuffer
integer, dimension(10800) :: i4datatemp
integer, dimension(1:sibdim(1),1:sibdim(2),0:num) :: ncount
Integer ilat,ilon,lci,lcj,nface,ctmp,ltmp,mthrng,imth,lrp,nlrp,k
integer ntmp,ix,iy,tix,tiy,vegtmp,i
integer ierr
integer, dimension(0:12), intent(in) :: ncid, varid
logical, dimension(0:12), intent(in) :: ncfile
character*2 cmth
Character*10 fname
character(len=*), intent(in) :: vegfilename, laifilename

if ( month==0 ) then
  mthrng=12
else
  mthrng=1
end if

coverout=0
countn=0

Write(6,*) "Read USGS + LAI data (stream)"

lrp=-1

Do ilat=1,21600

  if (mod(ilat,50).eq.0) then
    Write(6,*) 'USGS + LAI - ',ilat,'/ 21600'
  end if
  
  ! Read data
  if ( ncfile(0) ) then
    ierr = nf90_get_var(ncid(0),varid(0),i4databuffer,start=(/1,ilat/),count=(/43200,1/))
  else  
    Read(10,REC=ilat) databuffer
    i4databuffer = databuffer
  end if  
  aglat=callat(90.,ilat,10)

  ! read corrosponding lai data and fill to 1km grid
  nlrp=int(real(ilat+3)/4.)
  if (lrp/=nlrp) then
    lrp=nlrp
    do imth=1,mthrng
      if ( ncfile(imth) ) then
        ierr = nf90_get_var(ncid(imth),varid(imth),i4datatemp,start=(/1,lrp/),count=(/10800,1/))  
      else  
        read(10+imth,REC=lrp) datatemp
        i4datatemp = datatemp
      end if  
      do k=1,10800
        lbuff(4*k-3:4*k,imth)=i4datatemp(k)
      end do
    end do
  end if
    
  Do ilon=1,43200
    
    aglon=callon(-180.,ilon,10)
    
    Call lltoijmod(aglon,aglat,alci,alcj,nface)
    lci = nint(alci)
    lcj = nint(alcj)
    lcj = lcj+nface*sibdim(1)
    
    vegtmp=mod(i4databuffer(ilon)+256,256)
    if ( vegtmp==0 ) then
      ctmp=0
    else if ( mapjveg(max(min(vegtmp,class_num),1))==vegtmp ) then
      ctmp=max(min(vegtmp,class_num),1)
    else
      ctmp = -1
      do i = 1,class_num
        if ( vegtmp==mapjveg(i) ) then
          ctmp=i
          exit
        end if
      end do
    end if
    if (ctmp>=0.and.ctmp<=class_num) then
      coverout(lci,lcj,ctmp)=coverout(lci,lcj,ctmp)+1.
      ncount(lci,lcj,ctmp)=ncount(lci,lcj,ctmp)+1
      countn(lci,lcj)=1
      if ( ctmp>0 ) then
        do imth=1,mthrng
          ltmp=mod(lbuff(ilon,imth)+256,256)
          if (ltmp>0.and.ltmp<100) then
            coverout(lci,lcj,class_num+(ctmp-1)*mthrng+imth)=coverout(lci,lcj,class_num+(ctmp-1)*mthrng+imth)+real(ltmp)/10.
            ncount(lci,lcj,class_num+(ctmp-1)*mthrng+imth)=ncount(lci,lcj,class_num+(ctmp-1)*mthrng+imth)+1
          end if
        end do
      end if
    end if
    
  End Do
End Do
do lcj=1,sibdim(2)
  do lci=1,sibdim(1)
    ncount(lci,lcj,0:class_num)=sum(ncount(lci,lcj,0:class_num))
  end do
end do
coverout=coverout/real(max(ncount,1))

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads MODIS data at nscale=500m resolution
! (i.e., no storage, simply read and bin)
!

Subroutine modisstream(sibdim,coverout,countn,num,month,vegfilename,laifilename,class_num,mapjveg, &
                      ncid,varid,ncfile)

Use ccinterp
use netcdf_m

Implicit None

integer, intent(in) :: num,month,class_num
Integer, dimension(2), intent(in) :: sibdim
Real, dimension(1:sibdim(1),1:sibdim(2),0:num), intent(out) :: coverout
real, dimension(:,:,:), allocatable :: laiin
Real aglon,aglat,alci,alcj,bx,by,bdelta,tbx,tby,tbdelta
Real callon,callat
Integer, dimension(1:sibdim(1),1:sibdim(2)), intent(out) :: countn
integer, dimension(class_num), intent(in) :: mapjveg
Integer*1, dimension(1:86400) :: databuffer
Integer*1, dimension(1:21600) :: datatemp
Integer*1, dimension(1:86400,1:12) :: lbuff
integer, dimension(86400) :: i4databuffer
integer, dimension(21600) :: i4datatemp
integer, dimension(1:sibdim(1),1:sibdim(2),0:num) :: ncount
Integer ilat,ilon,lci,lcj,nface,ctmp,ltmp,mthrng,imth,lrp,nlrp,k
integer ntmp,ix,iy,tix,tiy,vegtmp,i
integer ierr
integer, dimension(0:12), intent(in) :: ncid, varid
logical, dimension(0:12), intent(in) :: ncfile
character*2 cmth
Character*10 fname
character(len=*), intent(in) :: vegfilename, laifilename

if ( month==0 ) then
  mthrng=12
else
  mthrng=1
end if

coverout=0
countn=0

Write(6,*) "Read MODIS + LAI data (stream)"

lrp=-1

Do ilat=1,43200

  if (mod(ilat,50).eq.0) then
    Write(6,*) 'USGS + LAI - ',ilat,'/ 43200'
  end if
  
  ! Read data
  ierr = nf90_get_var(ncid(0),varid(0),i4databuffer,start=(/1,ilat/),count=(/86400,1/))
  aglat=callat(90.,ilat,5)

  ! read corrosponding lai data and fill to 1km grid
  nlrp=int(real(ilat+7)/8.)
  if (lrp/=nlrp) then
    lrp=nlrp
    do imth=1,mthrng
      if ( ncfile(imth) ) then
        ierr = nf90_get_var(ncid(imth),varid(imth),i4datatemp,start=(/1,lrp/),count=(/10800,1/))  
      else  
        read(10+imth,REC=lrp) datatemp
        i4datatemp = datatemp
      end if  
      do k=1,10800
        lbuff(8*k-7:8*k,imth)=i4datatemp(k)
      end do
    end do
  end if
    
  Do ilon=1,86400
    
    aglon=callon(-180.,ilon,5)
    
    Call lltoijmod(aglon,aglat,alci,alcj,nface)
    lci = nint(alci)
    lcj = nint(alcj)
    lcj = lcj+nface*sibdim(1)
    
    vegtmp=mod(i4databuffer(ilon)+256,256)
    if ( vegtmp==0 ) then
      ctmp=0
    else if ( mapjveg(max(min(vegtmp,class_num),1))==vegtmp ) then
      ctmp=max(min(vegtmp,class_num),1)
    else
      ctmp = -1
      do i = 1,class_num
        if ( vegtmp==mapjveg(i) ) then
          ctmp=i
          exit
        end if
      end do
    end if
    if (ctmp>=0.and.ctmp<=class_num) then
      coverout(lci,lcj,ctmp)=coverout(lci,lcj,ctmp)+1.
      ncount(lci,lcj,ctmp)=ncount(lci,lcj,ctmp)+1
      countn(lci,lcj)=1
      if ( ctmp>0 ) then
        do imth=1,mthrng
          ltmp=mod(lbuff(ilon,imth)+256,256)
          if (ltmp>0.and.ltmp<100) then
            coverout(lci,lcj,class_num+(ctmp-1)*mthrng+imth)=coverout(lci,lcj,class_num+(ctmp-1)*mthrng+imth)+real(ltmp)/10.
            ncount(lci,lcj,class_num+(ctmp-1)*mthrng+imth)=ncount(lci,lcj,class_num+(ctmp-1)*mthrng+imth)+1
          end if
        end do
      end if
    end if
    
  End Do
End Do
do lcj=1,sibdim(2)
  do lci=1,sibdim(1)
    ncount(lci,lcj,0:class_num)=sum(ncount(lci,lcj,0:class_num))
  end do
end do
coverout=coverout/real(max(ncount,1))

Return
End

    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads soil at nscale_4=1
! (i.e., 4km) resolution.
! (i.e., no storage, simply read and bin)
!

Subroutine soilstream(sibdim,coverout,countn,soilfilename,ncid,varid,ncfile)

Use ccinterp
use netcdf_m

Implicit None

Integer, dimension(2), intent(in) :: sibdim
Real, dimension(sibdim(1),sibdim(2),0:8), intent(out) :: coverout
Real aglon,aglat,alci,alcj
Real callon,callat
Integer, dimension(sibdim(1),sibdim(2)), intent(out) :: countn
Integer*1, dimension(10800) :: databuffer
Integer, dimension(10800) :: i4databuffer
Integer ilat,ilon,lci,lcj,nface,cpos,i
integer ierr
integer, intent(in) :: ncid, varid
integer, dimension(0:13), parameter :: masmap=(/ 0, 1, 1, 4, 2, 4, 7, 2, 2, 5, 6, 3, 8, 9 /)
logical, intent(in) :: ncfile
character(len=*), intent(in) :: soilfilename

coverout=0
countn=0

Write(6,*) "Read HWSD data (stream)"

Do ilat=1,5400

  if (mod(ilat,50).eq.0) then
    Write(6,*) 'HWSD - ',ilat,'/ 5400'
  end if
  
  ! Read data
  if ( ncfile ) then
    ierr = nf90_get_var(ncid,varid,i4databuffer,start=(/1,ilat/),count=(/10800,1/)) 
  else    
    Read(20,REC=ilat) databuffer
    i4databuffer = databuffer
  end if  
  aglat=callat(90.,ilat,40)
  
  Do ilon=1,10800
    
    aglon=callon(-180.,ilon,40)
    
    Call lltoijmod(aglon,aglat,alci,alcj,nface)
    lci = nint(alci)
    lcj = nint(alcj)
    lcj = lcj+nface*sibdim(1)
    
    cpos=i4databuffer(ilon)
    if ((cpos.ge.1).and.(cpos.le.13)) then
      If (coverout(lci,lcj,0).LT.0.) then
        coverout(lci,lcj,:)=0.
        countn(lci,lcj)=0
      End If
      coverout(lci,lcj,masmap(cpos)-1)=coverout(lci,lcj,masmap(cpos)-1)+1.
      countn(lci,lcj)=countn(lci,lcj)+1
    else
      If (countn(lci,lcj).EQ.0) then
        coverout(lci,lcj,:)=-1.
        countn(lci,lcj)=1
      End if    
    end if
    
  End Do
End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads albedo at nscale_4=1
! (i.e., 4km) resolution.
! (i.e., no storage, simply read and bin)
!

Subroutine albedostream(sibdim,coverout,countn,datatype,albfilename,ncid,varid,ncfile)

Use ccinterp
use netcdf_m

Implicit None

Integer, dimension(2), intent(in) :: sibdim
Integer, dimension(sibdim(1),sibdim(2)), intent(out) :: countn
Integer, dimension(10800) :: databuffer
Integer*1, dimension(10800) :: datatemp
Integer ilat,ilon,lci,lcj,nface
integer ierr
integer, intent(in) :: ncid, varid
Real, dimension(sibdim(1),sibdim(2)), intent(out) :: coverout
Real aglon,aglat,alci,alcj
Real callon,callat
logical, intent(in) :: ncfile
Character(len=*), intent(in) :: datatype
Character(len=20) cmsg
character(len=*), intent(in) :: albfilename

coverout=0
countn=0

select case(datatype)
  case ('albvis')
    cmsg='Soil albedo (VIS) - '
  case ('albnir')
    cmsg='Soil albedo (NIR) - '
end select

Do ilat=1,5400
  aglat=callat(90.,ilat,40)

  if (mod(ilat,50).eq.0) then
    Write(6,*) cmsg,ilat,'/ 5400'
  end if
  
  ! Read data
  if ( ncfile ) then
    ierr = nf90_get_var(ncid,varid,databuffer,start=(/1,ilat/),count=(/10800,1/)) 
  else    
    Read(40,REC=ilat) datatemp
    databuffer = datatemp
  end if  
  
  where (databuffer<0)
    databuffer=databuffer+256
  end where
  
  Do ilon=1,10800
    aglon=callon(-180.,ilon,40)
    
    Call lltoijmod(aglon,aglat,alci,alcj,nface)
    lci = nint(alci)
    lcj = nint(alcj)
    lcj = lcj+nface*sibdim(1)
    
    If (databuffer(ilon).LE.100) then
      If (coverout(lci,lcj).LT.0.) then
        coverout(lci,lcj)=0.
        countn(lci,lcj)=0
      End If
      coverout(lci,lcj)=coverout(lci,lcj)+real(databuffer(ilon))/100.
      countn(lci,lcj)=countn(lci,lcj)+1
    Else
      If (countn(lci,lcj).EQ.0) then
        coverout(lci,lcj)=-1.
        countn(lci,lcj)=1
      End if
    End if    
  End Do
End Do

Return
End

    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads soil at nscale_30=1
! (i.e., 30km) resolution.
! (i.e., no storage, simply read and bin)
!

Subroutine changestream(sibdim,coverout,countn,changefilename,year,ncid)

Use ccinterp
use netcdf_m

Implicit None

integer, intent(in) :: year
Integer, dimension(2), intent(in) :: sibdim
Real, dimension(sibdim(1),sibdim(2),0:2), intent(out) :: coverout
Real aglon,aglat,alci,alcj
Real callon,callat,cpos
Integer, dimension(sibdim(1),sibdim(2)), intent(out) :: countn
real, dimension(1440) :: databuffer
Integer ilat,ilon,lci,lcj,nface,i,n,indx,iposa,iposb
integer varid, ierr, rectime, dimid, yyyy, dimlen
integer, intent(in) :: ncid
character(len=*), intent(in) :: changefilename
character(len=32) units

coverout=0
countn=0

Write(6,*) "Read change data (stream)"

ierr = nf90_inq_varid(ncid,"time",varid)
ierr = nf90_get_att(ncid,varid,"units",units)
iposa = index(trim(units),'since')
iposa = iposa + 5 ! skip 'since'
iposb = index(trim(units(iposa:)),'-')
iposb = iposb - 2 ! remove '-'
read(units(iposa:iposb),FMT=*,iostat=ierr) yyyy
if ( ierr/=0 ) then
  write(6,*) "ERROR: Cannot read time units. Expecting year but found ",units(iposa:iposb)
  call finishbanner
  stop -1
end if
rectime = year - yyyy + 1
ierr = nf90_inq_dimid(ncid,'time',dimid)
ierr = nf90_inquire_dimension(ncid,dimid,len=dimlen)
if ( rectime>dimlen .or. rectime<1 ) then
  write(6,*) "ERROR: year not found in input file"
  write(6,*) "year = ",year
  write(6,*) "file = ", trim(changefilename)
  call finishbanner
  stop -1
end if

Do ilat=1,720

  if (mod(ilat,50).eq.0) then
    Write(6,*) 'Change - ',n,'/ 7  ',ilat,'/ 720'
  end if

  do n = 1,7
    
    select case(n)
      case(1)
        ierr = nf90_inq_varid(ncid,"c3ann",varid)
        indx = 0
      case(2)
        ierr = nf90_inq_varid(ncid,"c4ann",varid)
        indx = 1
      case(3)
        ierr = nf90_inq_varid(ncid,"c3per",varid)
        indx = 0
      case(4)
        ierr = nf90_inq_varid(ncid,"c4per",varid)
        indx = 1
      case(5)
        ierr = nf90_inq_varid(ncid,"c3nfx",varid)
        indx = 0
      case(6)
        ierr = nf90_inq_varid(ncid,"pastr",varid)
        indx = 2
      case(7)
        ierr = nf90_inq_varid(ncid,"range",varid)
        indx = 2
    end select    

    ! Read data
    ierr = nf90_get_var(ncid,varid,databuffer,start=(/1,ilat,rectime/),count=(/1440,1,1/)) 
    aglat=callat(90.,ilat,300)
  
    Do ilon=1,1440
    
      aglon=callon(-180.,ilon,300)
    
      Call lltoijmod(aglon,aglat,alci,alcj,nface)
      lci = nint(alci)
      lcj = nint(alcj)
      lcj = lcj + nface*sibdim(1)
    
      cpos = databuffer(ilon)
      if ( cpos>=0. .or. cpos<=1. ) then
        coverout(lci,lcj,indx) = coverout(lci,lcj,indx) + cpos 
        countn(lci,lcj) = countn(lci,lcj) + 1
      end if
      
    End Do
    
  end do
  
End Do

Return
End
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine aligns the data with the requested lat/lon
!

Subroutine solvejshift(lonin,jin,jout,nscale)

Implicit None

Real, intent(in) :: lonin
Integer, intent(in) :: nscale ! actually related to the inverse of nscale
Integer, dimension(1:2,1:2), intent(out) :: jin,jout
Integer jshift

jshift=Mod(nint(lonin*real(nscale))+180*nscale,360*nscale)
If (jshift.LT.0) jshift=jshift+360*nscale
jin(1,1)=1
jout(1,1)=Mod(jin(1,1)+jshift,360*nscale)
If (jout(1,1).EQ.0) jout(1,1)=360*nscale
jout(1,2)=360*nscale
jin(1,2)=jout(1,2)-jout(1,1)+jin(1,1)
jin(2,1)=jin(1,2)+1
jin(2,2)=360*nscale
jout(2,1)=1
jout(2,2)=jout(1,1)-1
If (jin(2,1).GT.jin(2,2)) then
  jin(2,:)=jin(1,:)
  jout(2,:)=jout(1,:)
End if

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine groups tile data
!

Subroutine dataconvert(datain,raw,nscale,num)

Implicit None

Integer, intent(in) :: nscale,num
Integer, dimension(1:nscale,1:nscale), intent(in) :: datain
Real, dimension(0:num), intent(out) :: raw
Integer i,j,datatmp

! Aggregate land use
! Faster to step over grid once
raw=0.
Do j=1,nscale
  Do i=1,nscale
    datatmp=datain(i,j)
    datatmp=mod(datatmp+256,256)
    raw(datatmp)=raw(datatmp)+1.
  End Do
End Do
raw=raw/real(nscale**2)

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine converts from 1km nscale to adj km nscale
!

Subroutine kmconvert(nscale,nscale_x,lldim,lldim_x,adj)

Implicit None

Integer, intent(in) :: nscale,adj
Integer, intent(out) :: nscale_x
Integer, dimension(1:2), intent(in) :: lldim
Integer, dimension(1:2), intent(out) :: lldim_x
Integer i

nscale_x=Int(nscale/adj)
If (nscale_x.LT.1) nscale_x=1

Do i=1,2
  lldim_x(i)=Int(Real(lldim(i))*Real(nscale)/(Real(nscale_x)*real(adj)))
  If (lldim_x(i).LT.1) lldim_x(i)=1
End Do

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determines how many subsections (blocks) the data
! needs to be broken into so as to fit into memory (i.e., sibsize)
!

Subroutine scaleconvert(nscale,subsec,lldim,sll,sibsize)

Implicit None

Integer, intent(out) :: subsec
Integer, intent(in) :: nscale,sibsize
Integer, dimension(1:2), intent(out) :: lldim
Real, dimension(1:2,1:2), intent(in) :: sll
Integer i,j

i=nint((sll(1,2)-sll(1,1))*1200./Real(nscale))
j=nint((sll(2,2)-sll(2,1))*1200./Real(nscale))

subsec=int(sqrt(real(i)*real(j)/(real(sibsize)**2)))+1
subsec=max(subsec,1)
  
lldim(1)=nint(real(i)/real(subsec))
lldim(2)=nint(real(j)/real(subsec))

! Backwards compatibility?
if ( mod(nscale,10)==0 ) then
  If (real(lldim(1)*(nscale/10)*subsec).LT.(sll(1,2)-sll(1,1))*120.) lldim(1)=lldim(1)+1
  If (real(lldim(2)*(nscale/10)*subsec).LT.(sll(2,2)-sll(2,1))*120.) lldim(2)=lldim(2)+1
else    
  If (real(lldim(1)*nscale*subsec).LT.(sll(1,2)-sll(1,1))*1200.) lldim(1)=lldim(1)+1
  If (real(lldim(2)*nscale*subsec).LT.(sll(2,2)-sll(2,1))*1200.) lldim(2)=lldim(2)+1
end if  
If ((nint((90.-sll(2,2))*1200.)+lldim(2)*nscale).GT.216000) lldim(2)=(216000-nint((90.-sll(2,2))*1200.))/nscale
If ((lldim(1)*nscale).GT.432000) lldim(1)=432000/nscale

If ((lldim(1).LT.1).OR.(lldim(2).LT.1)) Then
  lldim=(/ 0, 0 /)
  subsec=0
End If

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine adjusts the latlon array for a specified subsection
!

Subroutine latlonconvert(nscale,latlon,lldim,slonn,slatx,nx,ny)

Implicit None

Integer, intent(in) :: nscale
Real, dimension(1:2), intent(out) :: latlon
Integer, dimension(1:2), intent(in) :: lldim
Real, intent(in) :: slonn,slatx
Integer, intent(in) :: nx,ny

latlon=(/ slonn+Real((nx-1)*lldim(1)*nscale)/1200., slatx-Real((ny-1)*lldim(2)*nscale)/1200. /)
if (latlon(2).LT.-90.) latlon(2)=-90.

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function calculates lon from an array index
!

Real function callon(latlon,i,nscale)

Implicit None

Real, intent(in) :: latlon
Integer, intent(in) :: i,nscale

callon=(Real(i-1)+0.5)*real(nscale)/1200.+latlon

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function rounds up
!

Integer Function rndup(x)

Implicit None

Real, intent(in) :: x

rndup=int(x)
if (x>real(rndup)) rndup=rndup+1

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function calculates lat from an array index
!

Real function callat(latlon,i,nscale)

Implicit None

Real, intent(in) :: latlon
Integer, intent(in) :: i,nscale

callat=latlon-(Real(i-1)+0.5)*real(nscale)/1200.

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function calculates the array index for a specified lon
!

Real function indexlon(aglon,latlon,nscale)

Implicit None

Real, intent(in) :: aglon,latlon
Integer, intent(in) :: nscale

indexlon=(aglon-latlon)*1200./real(nscale)+0.5
	    
Return
End	    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function calculates the array index for a specified lat
!

Real function indexlat(aglat,latlon,nscale)

Implicit None

Real, intent(in) :: aglat,latlon
Integer, intent(in) :: nscale
   
indexlat=(-aglat+latlon)*1200./real(nscale)+0.5

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine determines the boundary of a subsection for a
! specified scale.
!

Subroutine searchdim(mode,sll,nscale,scalelimit,latlon,lldim,grid,maskn,rlld,sibdim)

Implicit None

Integer, intent(in) :: mode,nscale
Integer, dimension(1:2), intent(in) :: lldim,sibdim
Real, intent(in) :: scalelimit
Real, dimension(1:2,1:2), intent(out) :: sll
Real, dimension(1:2), intent(in) :: latlon
Real, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: grid
Real, dimension(1:sibdim(1),1:sibdim(2),1:2), intent(in) :: rlld
Real, dimension(1:sibdim(1),1:sibdim(2),1:2) :: tlld
Real, dimension(1:2,1:2) :: templl
Integer, dimension(1:2,1:2,1:2) :: posll
Integer i,j
Integer rndup
Logical, dimension(1:sibdim(1),1:sibdim(2)) :: sermask
Logical, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: maskn

tlld=rlld

templl(1,1)=latlon(1)
templl(1,2)=latlon(1)+real(lldim(1)*nscale)/1200.
templl(2,2)=latlon(2)
templl(2,1)=latlon(2)-real(lldim(2)*nscale)/1200.

Do i=1,2
  If (templl(2,i).LT.-90.) templl(2,i)=-90.
  If (templl(2,i).GT.90.) templl(2,i)=90.
End Do

sermask=(tlld(:,:,1).GE.templl(1,1)).AND.(tlld(:,:,1).LE.templl(1,2)) &
        .AND.(tlld(:,:,2).GE.templl(2,1)).AND.(tlld(:,:,2).LE.templl(2,2))
sermask=sermask.AND.maskn

Select Case(mode)
  Case(0)
    ! Use all grid points
    sll=templl
    Return
  Case(1)
    sermask=sermask.AND.(grid*10.<=scalelimit)
  Case(2)
    sermask=sermask.AND.(grid*10.>=scalelimit)
  Case(3)
    sermask=sermask.AND.(grid*10.==scalelimit)
  Case(4)
    ! Do nothing
  Case Default
    Write(6,*) 'ERROR: Internal error.  Unsupported mode in searchdim'
    call finishbanner
    Stop -1
End Select

If (.NOT.Any(sermask)) then
  sll=0.
  Return
End if

sll(1,2)=Maxval(tlld(:,:,1),sermask)
sll(1,1)=Minval(tlld(:,:,1),sermask)
sll(2,2)=Maxval(tlld(:,:,2),sermask)
sll(2,1)=Minval(tlld(:,:,2),sermask)

posll(1,2,:)=Maxloc(tlld(:,:,1),sermask)
posll(1,1,:)=Minloc(tlld(:,:,1),sermask)
posll(2,2,:)=Maxloc(tlld(:,:,2),sermask)
posll(2,1,:)=Minloc(tlld(:,:,2),sermask)
Do i=1,2
  Do j=1,2
    ! 1.6 is assumed to span from the centre to the corner (i.e., sqrt(2) if
    ! using a square grid)
    sll(i,j)=sll(i,j)+Real(j*2-3)*grid(posll(i,j,1),posll(i,j,2))*1.6/240.
  End Do
End Do

sll(1,1)=real(int((sll(1,1)-latlon(1))*1200./real(nscale)))*real(nscale)/1200.+latlon(1)
sll(1,2)=real(rndup((sll(1,2)-latlon(1))*1200./real(nscale)))*real(nscale)/1200.+latlon(1)
sll(2,1)=-real(rndup((latlon(2)-sll(2,1))*1200./real(nscale)))*real(nscale)/1200.+latlon(2)
sll(2,2)=-real(int((latlon(2)-sll(2,2))*1200./real(nscale)))*real(nscale)/1200.+latlon(2)

! Check bounds
Do i=1,2
  sll(i,1)=max(sll(i,1),templl(i,1))
  sll(i,2)=min(sll(i,2),templl(i,2))
End Do

! Consistancy check
If ((sll(1,1).GT.sll(1,2)).OR.(sll(2,1).GT.sll(2,2))) then
  sll=0.
End If

Return
End


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine calculates the next scale to load.  The suboutine
! attempts to minimise the number of times the sibveg data is loaded.
!

Subroutine findsmallscale(nscale,scalelimit,latlon,llstore,grid,maskn,rlld,subsec,sll,sibsize,sibdim)

Implicit None

Integer, intent(in) :: sibsize,scalelimit
Integer, dimension(1:2), intent(in) :: sibdim
Integer, intent(inout) :: nscale
Integer, intent(out) :: subsec
Integer, dimension(1:2), intent(out) :: llstore
Real, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: grid
Real, dimension(1:sibdim(1),1:sibdim(2),1:2), intent(in) :: rlld
Real, dimension(1:2), intent(in) :: latlon
Real, dimension(1:2,1:2), intent(out) :: sll
Real tscale
Integer mode,maxscale,subsecmax
Integer findfact
Logical, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: maskn

tscale=Maxval(grid,maskn)*10.

mode=1
If (nscale==9999) mode=0

maxscale=Int(0.5*real(nscale)/Real(scalelimit))*scalelimit
maxscale=findfact(216000,maxscale,-scalelimit)
maxscale = max( maxscale, scalelimit )

llstore=(/ 432000/maxscale , 216000/maxscale /)
Call searchdim(mode,sll,maxscale,tscale,latlon,llstore,grid,maskn,rlld,sibdim)
Call scaleconvert(maxscale,subsecmax,llstore,sll,sibsize)

If (subsecmax<1) Then
  Write(6,*) "WARN: Cannot locate unassigned points in findsmallscale"
  mode=0
  nscale=maxscale
Else
  nscale=Int(Minval(grid,maskn)*10./Real(scalelimit))*scalelimit
  nscale=findfact(216000,nscale,-scalelimit)
  nscale=max(nscale,scalelimit)
  subsec=subsecmax+1
  Do While (subsec>subsecmax)
    ! Get estimate of array size
    llstore=(/ 432000/nscale , 216000/nscale /)
    ! Calculate domain for search
    Call searchdim(mode,sll,nscale,tscale,latlon,llstore,grid,maskn,rlld,sibdim)
    ! Define number of points in domain and subdivide into tiles if array is too big
    Call scaleconvert(nscale,subsec,llstore,sll,sibsize)
    If (subsec>subsecmax) Then
      nscale=nscale+scalelimit
      nscale=findfact(216000,nscale,scalelimit)
    End If
  End Do
End If

nscale=min(nscale,maxscale)
nscale=max(nscale,scalelimit)


llstore=(/ 432000/nscale , 216000/nscale /)
! Calculate domain for search
Call searchdim(mode,sll,nscale,tscale,latlon,llstore,grid,maskn,rlld,sibdim)
! Define number of points in domain and subdivide into tiles if array is too big
Call scaleconvert(nscale,subsec,llstore,sll,sibsize)

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This function finds the nearest factor of an integer
!

Integer Function findfact(x,y,delta)

Implicit None

Integer, intent(in) :: x,y,delta
Integer z

z=y

If (z==0) Then
  findfact=1
  Return
End If

Do While (Mod(x,z)/=0.)
  z=z+delta
  z=max(z,1)
  z=min(z,x)
End Do

findfact=z

Return
End

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculate zobler soil texture from FAO
!

subroutine calsoilnear(landdata,soildata,lsdata,sibdim,tdata,class_num,mapwater,mapice)

implicit none

integer, intent(in) :: class_num
integer, dimension(1:2), intent(in) :: sibdim
integer, dimension(1:sibdim(1),1:sibdim(2)), intent(out) :: tdata
real, dimension(1:sibdim(1),1:sibdim(2),0:17), intent(in) :: landdata
real, dimension(1:sibdim(1),1:sibdim(2),0:8), intent(in) :: soildata
real, dimension(1:sibdim(1),1:sibdim(2)), intent(in) :: lsdata
real testdata
integer ilon,ilat,pos(1),i
logical, dimension(class_num), intent(in) :: mapwater, mapice

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(ilat,ilon,pos,testdata)
do ilat=1,sibdim(2)
  do ilon=1,sibdim(1)
    pos=Maxloc(landdata(ilon,ilat,1:class_num),.not.mapwater)
    if (1-nint(lsdata(ilon,ilat))==0) then
      testdata=sum(landdata(ilon,ilat,1:class_num),mapwater)
      if (landdata(ilon,ilat,0)>=testdata) then
        tdata(ilon,ilat)=0  ! ocean
      else
        tdata(ilon,ilat)=-1 ! in-land water  
      end if
    else if ( mapice(pos(1)) ) then
      tdata(ilon,ilat)=9 ! ice
    else
      pos=Maxloc(soildata(ilon,ilat,:))
      tdata(ilon,ilat)=pos(1)
    end if
  end do
end do
!$OMP END PARALLEL DO

return
end

subroutine modifylanddata(dataout,glonlat,sibdim,num,month,datafilename,laifilename,class_num, &
                          mapjveg,grid,ovegfrac)

use ccinterp
use netcdf_m

implicit none

integer, intent(in) :: num, month, class_num
logical, intent(in) :: ovegfrac
integer, dimension(2), intent(in) :: sibdim
integer, dimension(2) :: dimid, dimlen 
integer, dimension(3) :: dimid_lai, dimlen_lai
integer, dimension(3) :: start, ncount
integer, dimension(sibdim(1),sibdim(2)) :: countlocal
integer, dimension(class_num), intent(in) :: mapjveg
integer, dimension(:,:,:), allocatable :: lcmap
integer ncid, ncidlai, varid, ierr
integer lci, lcj, nface, iveg, imonth
integer i, j, vegtmp, k, varndims
integer missing_int
integer jj
real, dimension(sibdim(1),sibdim(2),0:num), intent(inout) :: dataout
real, dimension(sibdim(1),sibdim(2),0:num) :: datalocal
real, dimension(sibdim(1),sibdim(2),12) :: lailocal
real, dimension(sibdim(1),sibdim(2)), intent(in) :: grid
real, dimension(2), intent(in) :: glonlat
real, dimension(:), allocatable :: lonin, latin, lonin_lai, latin_lai
real, dimension(:,:,:), allocatable :: laiin
real aglon, aglat, alci, alcj
real missing_real
integer, dimension(:,:), allocatable :: coverin
character(len=*), intent(in) :: datafilename, laifilename
character(len=1024), dimension(3) :: dimname
character(len=1024) varname
logical found_missing

!---------- using frac ------------
integer, dimension(sibdim(1),sibdim(2),0:num) :: countfrac
integer ilcn, jlcn, ilcx, jlcx, ivegfrac,ivegstart,ivegend
real lcinn, lcjnn, lcixx, lcjxx
real dx,dy,dgridin
real, parameter :: circe=40075.
integer numclassin,inc
integer diaglci, diaglcj
Character(len=40) cmsg

diaglci=-1
diaglcj=-1

!---------- using frac ------------

missing_int = -1
missing_real = -1.

! Veg file parameters
if ( datafilename/='' ) then
  ierr = nf_open(datafilename,nf_nowrite,ncid)
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot open user veg file ",trim(datafilename)
    call finishbanner
    stop -1
  end if
  
  ierr = nf_inq_varid(ncid,'land_cover',varid)
  if ( ierr/=nf_noerr ) then
    varid = 1
    ierr = nf_inq_varname(ncid,varid,varname)
  end if  
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot locate land_cover variable in veg file ",trim(datafilename)
    call finishbanner
    stop -1
  end if
  found_missing = .false.
  ierr = nf_get_att_int(ncid,varid,"missing_value",missing_int)
  if ( ierr==nf_noerr ) then
    found_missing = .true.
  else
    ierr = nf_get_att_int(ncid,varid,"_FillValue",missing_int) 
    if ( ierr==nf_noerr ) then
      found_missing = .true.  
    end if    
  end if   
  ierr = nf_inq_vardimid(ncid,varid,dimid(1:2))
  ierr = nf_inq_dim(ncid,dimid(1),dimname(1),dimlen(1))
  ierr = nf_inq_dim(ncid,dimid(2),dimname(2),dimlen(2))
  ierr = nf_inq_varndims(ncid,varid,varndims)
  write(6,*) "Reading land_cover data from ",trim(datafilename)
  write(6,*) "Found size ",dimlen(1:2)
  if ( found_missing ) then
    write(6,*) "Found missing value ",missing_int 
  end if    

  allocate( coverin(dimlen(1),dimlen(2)), lonin(dimlen(1)), latin(dimlen(2)) )
  allocate( lcmap(dimlen(1),dimlen(2),2) )

  select case(varndims)
    case(2)
      start(1:2) = 1
      ncount(1:2) = dimlen(1:2)
      ierr = nf_get_vara_int(ncid,varid,start(1:2),ncount(1:2),coverin) ! here land_cover_type
    case(3)
      start(1:3) = 1
      ncount(1:2) = dimlen(1:2)
      ncount(3) = 1
      ierr = nf_get_vara_int(ncid,varid,start(1:3),ncount(1:3),coverin) ! here land_cover_type
    case default
      write(6,*) "ERROR: Cannot process land_cover data with ndims ",varndims
      call finishbanner
      stop -1
  end select
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot read land_cover data"
    write(6,*) nf_strerror(ierr)
    call finishbanner
    stop -1
  end if
  ierr = nf_inq_varid(ncid,trim(dimname(1)),varid)
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot locate coordinate information for ",trim(dimname(1))
    call finishbanner
    stop -1
  end if
  ierr = nf_get_vara_real(ncid,varid,start(1:1),ncount(1:1),lonin)
  ierr = nf_inq_varid(ncid,trim(dimname(2)),varid)
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot locate coordinate information for ",trim(dimname(2))
    call finishbanner
    stop -1
  end if
  ierr = nf_get_vara_real(ncid,varid,start(2:2),ncount(2:2),latin)
  write(6,*) "Found longitude range ",lonin(1),lonin(dimlen(1))
  write(6,*) "Found latitude range  ",latin(1),latin(dimlen(2))
  
!---------- using frac ------------
! clear copunter for number of veg fraction hits per veg type
  countfrac(:,:,:) = 0

!---------- using frac ------------
  ! Process land-cover
  write(6,*) "Processing land-cover data for ovegfrac = ",ovegfrac
  datalocal(:,:,:) = 0. ! accumulation array per grid point and veg type
  countlocal(:,:) = 0

  !---------- using frac ------------
  ivegstart=-1
  ivegend=-1
  numclassin=0
  if ( ovegfrac ) then
    ivegstart=0
    ivegend=class_num
  endif
  do ivegfrac = ivegstart,ivegend
    select case(ivegfrac)
      case(-1)
        cmsg='land_cover'
        ierr = nf_inq_varid(ncid,cmsg,varid)
        if ( ierr/=nf_noerr ) then
          varid = 1
          ierr = nf_inq_varname(ncid,varid,cmsg)      
        end if
      case(0)
        cmsg='water'
        numclassin=numclassin+1
      case(1)
        cmsg='evergreen_needleleaf_forest'
        numclassin=numclassin+1
      case(2)
        cmsg='evergreen_broadleaf_forest'
        numclassin=numclassin+1
      case(3)
        cmsg='deciduous_needleleaf_forest'
        numclassin=numclassin+1
      case(4)
        cmsg='deciduous_broadleaf_forest'
        numclassin=numclassin+1
      case(5)
        cmsg='mixed_forest'
        numclassin=numclassin+1
      case(6)
        cmsg='closed_shrublands'
        numclassin=numclassin+1
      case(7)
        cmsg='open_shrublands'
        numclassin=numclassin+1
      case(8)
        cmsg='woody_savannas'
        numclassin=numclassin+1
      case(9)
        cmsg='savannas'
        numclassin=numclassin+1
      case(10)
        cmsg='grasslands'
        numclassin=numclassin+1
      case(11)
        cmsg='permanent_wetlands'
        numclassin=numclassin+1
      case(12)
        cmsg='croplands'
        numclassin=numclassin+1
      case(13)
        cmsg='urban_and_builtup'
        numclassin=numclassin+1
      case(14)
        cmsg='cropland_natural_vegetation_mosaic'
        numclassin=numclassin+1
      case(15)
        cmsg='snowandice'
        numclassin=numclassin+1
      case(16)
        cmsg='barren_sparsely_vegetated'
        numclassin=numclassin+1
      case(17)
        cmsg='unclassified_filled'
        numclassin=numclassin+1
    end select
    write(6,*)"#########################################cmsg=",trim(cmsg)
    write(6,*)"ivegfrac,numclassin=",ivegfrac,numclassin

    ierr = nf_inq_varid(ncid,cmsg,varid)
    write(6,*)"ierr, varid=",ierr , varid
    if ( ierr/=nf_noerr ) then
      write(6,*) "ERROR: Cannot locate ",cmsg," field in veg file ",trim(datafilename)
      call finishbanner
      stop -1
    end if
    write(6,*)"varndims, ivegfrac=",varndims, ivegfrac
    select case(varndims)
      case(2)
        start(1:2) = 1
        ncount(1:2) = dimlen(1:2)
        ierr = nf_get_vara_int(ncid,varid,start(1:2),ncount(1:2),coverin) ! here % cover for this veg type
      case(3)
        start(1:3) = 1
        ncount(1:2) = dimlen(1:2)
        ncount(3) = 1
        ierr = nf_get_vara_int(ncid,varid,start(1:3),ncount(1:3),coverin) ! here % cover for this veg type
      case default
        write(6,*) "ERROR: Cannot process data with ndims ",varndims
        call finishbanner
        stop -1
    end select

    ! Process user specified land-cover
    write(6,*) "Processing user specified land-cover data ivegfrac=",ivegfrac,sibdim(1)

    !---------- using frac ------------
    ! loop over all datappoints in user landcover dataset
    ! here assumes regular lon/lat grid
    if ( ovegfrac ) then
      !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) SHARED(dimlen,latin,lonin,sibdim,lcmap) &
      !$OMP   PRIVATE(j,aglat,i,aglon,alci,alcj,nface,lci,lcj)
      do j = 1,dimlen(2)
        aglat = latin(j)
        do i = 1,dimlen(1)
          aglon = lonin(i)  
          call lltoijmod(aglon,aglat,alci,alcj,nface)
          lci = nint(alci)
          lcj = nint(alcj)
          lcj = lcj + nface*sibdim(1)
          lcmap(i,j,1) = lci
          lcmap(i,j,2) = lcj
        end do
      end do
      !$OMP END PARALLEL DO  

      !$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(jj,j,i,lci,lcj,jlcn,jlcx) &
      !$OMP   PRIVATE(ilcn,ilcx,dx,dy,dgridin,inc,vegtmp,iveg,k)
      do jj = 1,sibdim(2)
        do j = 1,dimlen(2)
          do i = 1,dimlen(1)
            lci = lcmap(i,j,1)
            lcj = lcmap(i,j,2)
            if ( lcj==jj ) then
              !---------- using frac ------------
              !tmp index holders
              jlcn=max(j-1,1)
              jlcx=min(j+1,dimlen(2))
              ilcn=max(i-1,1)
              ilcx=min(i+1,dimlen(1))

              ! assuming lon/lat input grid
              ! dy = dlat*circe/360.
              ! dx = cos(lat)*dlon*circe/360.
              dy = circe*abs(latin(jlcx)-latin(jlcn))/(360.*real(jlcx-jlcn))
              dx = cos(aglat*3.14159/180.)*circe*abs(lonin(ilcx)-lonin(ilcn))/(360.*real(ilcx-ilcn))
              dgridin=max(dx,dy)
              ! how much to increment (+/-) to ensure finding data within model grid box
              !i.e. need to search at least 5 grid points of input 5km grid for model 1 km grid
              inc=int(dgridin/grid(lci,lcj)/2)+1
              ilcn=max(1,lci-inc)
              ilcx=min(sibdim(1),lci+inc)
              jlcn=max(1,lcj-nface*sibdim(1)-inc)+nface*sibdim(1)
              jlcx=min(sibdim(1),lcj-nface*sibdim(1)+inc)+nface*sibdim(1)

              !==============================================
              do lcj = min(jlcn , jlcx),max(jlcn , jlcx)
                do lci = min(ilcn , ilcx),max(ilcn , ilcx)
                  !==============================================
                  vegtmp = ivegfrac ! if using veg fractions - index of vegtype, specified in loop
                  !---------- using frac ------------
                  if ( lci .eq. diaglci .and. lcj .eq. diaglcj ) then
                    write(6,*)"inc=",inc,dgridin,dx,dy
                    write(6,*)"inx,jnx=",ilcn,ilcx,jlcn,jlcx
                  endif
                  if ( vegtmp==0 ) then
                    iveg=0
                  else if ( mapjveg(max(min(vegtmp,class_num),1))==vegtmp ) then
                    iveg=max(min(vegtmp,class_num),1)
                  else if ( found_missing .and. vegtmp==missing_int ) then
                    ! missing
                    iveg = -1 
                  else
                    iveg = -1
                    do k = 1,class_num
                      if ( mapjveg(k)==coverin(i,j) ) then
                        iveg = k
                        exit
                      end if
                    end do
                    if ( iveg==-1 ) then
                      write(6,*) "ERROR: land_cover data is not defined in mapping data"
                      write(6,*) "Invalid land_cover index ",iveg,coverin(i,j)," at lat,lon=",latin(j),lonin(i)
                      write(6,*) "Valid indices are ",mapjveg(1:class_num)
                      call finishbanner
                      stop -1
                    end if
                  end if
                  !---------- using frac ------------
                  if ( iveg>=0 .and. iveg<=class_num ) then
                    if ( coverin(i,j) .le. 100 ) then ! to avoid 255 when not defined, here coverin is % for this vegtype
                      countlocal(lci,lcj) = countlocal(lci,lcj) + 1 ! total number of hits for this gridbox
                      datalocal(lci,lcj,iveg) = datalocal(lci,lcj,iveg) + coverin(i,j)/100. ! here coverin is % for this vegtype
                      countfrac(lci,lcj,iveg) = countfrac(lci,lcj,iveg) + 1 ! number of hits for this gribbox and vegtype
                    endif
                  end if
                  if ( lci .eq. diaglci .and. lcj .eq. diaglcj ) then
                    write(6,*)"lci,lcj,iveg,countlocal,datalocal,countfrac"
                    write(6,*)lci,lcj,iveg,countlocal(lci,lcj),datalocal(lci,lcj,iveg),countfrac(lci,lcj,iveg)
                    write(6,*)"coverin,i,j=",coverin(i,j),i,j,aglon,aglat
                  endif

                end do ! ilc
              end do ! jlc
              !---------- using frac ------------
            end if 
          end do ! i
        end do ! j
        if ( mod(jj,sibdim(2)/10)==0 .or. jj==sibdim(2) ) then
          write(6,*) "User land_cover ",jj,"/",sibdim(2)
        end if
      end do   
      !$OMP END PARALLEL DO      

    else ! ovegfrac ..else..
  
      !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) SHARED(dimlen,latin,lonin,sibdim,lcmap) &
      !$OMP   PRIVATE(j,aglat,i,aglon,alci,alcj,nface,lci,lcj)
      do j = 1,dimlen(2)
        aglat = latin(j)
        do i = 1,dimlen(1)
          aglon = lonin(i)  
          call lltoijmod(aglon,aglat,alci,alcj,nface)
          lci = nint(alci)
          lcj = nint(alcj)
          lcj = lcj + nface*sibdim(1)
          lcmap(i,j,1) = lci
          lcmap(i,j,2) = lcj
        end do
      end do
      !$OMP END PARALLEL DO
      
      !$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(jj,j,i,lci,lcj,nface,vegtmp) &
      !$OMP   PRIVATE(iveg,k)
      do jj = 1,sibdim(2)
        do j = 1,dimlen(2)
          do i = 1,dimlen(1)
            lci = lcmap(i,j,1)
            lcj = lcmap(i,j,2)
            if ( lcj==jj ) then
              !==============================================
              vegtmp = coverin(i,j)  ! if using landtype - index of vegtype, from read 
              if ( vegtmp==0 ) then
                iveg=0
              else if ( mapjveg(max(min(vegtmp,class_num),1))==vegtmp ) then
                iveg=max(min(vegtmp,class_num),1)
              else if ( found_missing .and. vegtmp==missing_int ) then
                ! missing
                iveg = -1 
              else
                iveg = -1
                do k = 1,class_num
                  if ( mapjveg(k)==coverin(i,j) ) then
                    iveg = k
                    exit
                  end if
                end do
                if ( iveg==-1 ) then
                  write(6,*) "ERROR: land_cover data is not defined in mapping data"
                  write(6,*) "Invalid land_cover index ",iveg,coverin(i,j)," at lat,lon=",latin(j),lonin(i)
                  write(6,*) "Valid indices are ",mapjveg(1:class_num)
                  call finishbanner
                  stop -1
                end if
              end if
              if ( iveg>=0 ) then
                datalocal(lci,lcj,iveg) = datalocal(lci,lcj,iveg) + 1. 
                countlocal(lci,lcj) = countlocal(lci,lcj) + 1
              else !if ( sum(datalocal(lci,lcj,:))>0. ) then
                ! missing value.
                datalocal(lci,lcj,1:class_num) = datalocal(lci,lcj,1:class_num) + dataout(lci,lcj,1:class_num)  
                countlocal(lci,lcj) = countlocal(lci,lcj) + 1
              end if 

            end if  
          end do ! i
        end do ! j  
        if ( mod(jj,sibdim(2)/10)==0 .or. jj==sibdim(2) ) then
          write(6,*) "User land_cover ",jj,"/",sibdim(2)
        end if
      end do
      !$OMP END PARALLEL DO
  
    end if
  
  enddo ! ivegfrac=0,class_num
  
  ! Average LAI before redistributing vegetation classes
  lailocal(:,:,:) = 0.
  if ( month==0 ) then
    do imonth = 1,12
      do iveg = 1,class_num
        lailocal(:,:,imonth) = lailocal(:,:,imonth) + dataout(:,:,iveg)*dataout(:,:,class_num+(iveg-1)*12+imonth) 
      end do
      do iveg = 1,class_num
        where ( countlocal(:,:)>0 )
          dataout(:,:,class_num+(iveg-1)*12+imonth) = lailocal(:,:,imonth)
        end where
      end do
    end do
  else
    do iveg = 1,class_num
      lailocal(:,:,month) = lailocal(:,:,month) + dataout(:,:,iveg)*dataout(:,:,class_num+(iveg-1)+1)
    end do
    do iveg = 1,class_num
      where ( countlocal(:,:)>0 )
        dataout(:,:,class_num+(iveg-1)+1) = lailocal(:,:,month)
      end where
    end do
  end if
  
  ! replace dataout with non-trival input data
  if ( ovegfrac ) then
    do iveg = 0,class_num
      where ( countfrac(:,:,iveg)>0 )
        dataout(:,:,iveg) = datalocal(:,:,iveg)/real(countfrac(:,:,iveg))
      end where
      if ( diaglci>=1 .and. diaglci<=sibdim(1) ) then
        if ( diaglcj>=1 .and. diaglcj<=sibdim(2) ) then  
          write(6,*)"compute iveg,dataout=",iveg,dataout(diaglci,diaglcj,iveg)
          write(6,*)"countlocal,datalocal,countfrac"
          write(6,*)countlocal(diaglci,diaglcj),datalocal(diaglci,diaglcj,iveg),countfrac(diaglci,diaglcj,iveg)
        end if
      end if  
    end do  
  else
    do iveg = 0,class_num     
      where ( countlocal(:,:)>0 )
        dataout(:,:,iveg) = datalocal(:,:,iveg)/real(countlocal(:,:))
      end where
    end do ! iveg
  end if
    
  deallocate( coverin, lonin, latin )
  deallocate( lcmap )
  ierr = nf_close(ncid)
  
end if
  

! LAI file parameters
if ( laifilename/='' ) then
  ierr = nf_open(laifilename,nf_nowrite,ncidlai)
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot open user lai file ",trim(laifilename)
    call finishbanner
    stop -1
  end if
  ierr = nf_inq_varid(ncidlai,'LAI',varid)
  if ( ierr/=nf_noerr ) ierr = nf_inq_varid(ncidlai,'lai',varid)
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot locate LAI variable in lai file ",trim(laifilename)
    call finishbanner
    stop -1
  end if
  found_missing = .false.
  ierr = nf_get_att_real(ncidlai,varid,"missing_value",missing_real)
  if ( ierr==nf_noerr ) then
    found_missing = .true.
  else
    ierr = nf_get_att_real(ncidlai,varid,"_FillValue",missing_real) 
    if ( ierr==nf_noerr ) then
      found_missing = .true.  
    end if    
  end if   
  if ( found_missing ) then
    write(6,*) "Found missing value ",missing_real
  else
    write(6,*) "Using default missing value ",missing_real  
  end if 
  ierr = nf_inq_vardimid(ncidlai,varid,dimid_lai(1:3))
  ierr = nf_inq_dim(ncidlai,dimid_lai(1),dimname(1),dimlen_lai(1))
  ierr = nf_inq_dim(ncidlai,dimid_lai(2),dimname(2),dimlen_lai(2))
  ierr = nf_inq_dim(ncidlai,dimid_lai(3),dimname(3),dimlen_lai(3))
  write(6,*) "Reading land_cover data from ",trim(laifilename)
  write(6,*) "Found size ",dimlen_lai(1:3)
  if ( month==0 ) then
    if ( dimlen_lai(3)/=12 ) then
      write(6,*) "ERROR: Require 12 months of LAI data when using month=0"
      call finishbanner
      stop -1
    end if
    start(3) = 1
    ncount(3) = 12
  else
    if ( dimlen_lai(3)==12 ) then
      start(3) = month
      ncount(3) = 1
      dimlen_lai(3) = 1
    else
      start(3) = 1
      ncount(3) = 1
    end if
  end if
  allocate( laiin(dimlen_lai(1),dimlen_lai(2),dimlen_lai(3)) )
  allocate( lonin_lai(dimlen_lai(1)), latin_lai(dimlen_lai(2)) )
  allocate( lcmap(dimlen_lai(1),dimlen_lai(2),2) )
  start(1:2) = 1
  ncount(1:2) = dimlen_lai(1:2)
  ierr = nf_get_vara_real(ncidlai,varid,start(1:3),ncount(1:3),laiin)
  ierr = nf_inq_varid(ncidlai,trim(dimname(1)),varid)
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot locate coordinate information for ",trim(dimname(1))
    call finishbanner
    stop -1
  end if
  ierr = nf_get_vara_real(ncidlai,varid,start(1:1),ncount(1:1),lonin_lai)
  ierr = nf_inq_varid(ncidlai,trim(dimname(2)),varid)
  if ( ierr/=nf_noerr ) then
    write(6,*) "ERROR: Cannot locate coordinate information for ",trim(dimname(2))
    call finishbanner
    stop -1
  end if
  ierr = nf_get_vara_real(ncidlai,varid,start(2:2),ncount(2:2),latin_lai)

  ! Process user defined input data
  write(6,*) "Processing LAI data"
  lailocal(:,:,:) = 0.
  countlocal(:,:) = 0
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) SHARED(dimlen_lai,latin_lai,lonin_lai,sibdim,lcmap) &
!$OMP  PRIVATE(j,aglat,i,aglon,alci,alcj,nface,lci,lcj)
  do j = 1,dimlen_lai(2)
    aglat = latin_lai(j)
    do i = 1,dimlen_lai(1)
      aglon = lonin_lai(i)  
      call lltoijmod(aglon,aglat,alci,alcj,nface)
      lci = nint(alci)
      lcj = nint(alcj)
      lcj = lcj+nface*sibdim(1)
      lcmap(i,j,1) = lci
      lcmap(i,j,2) = lcj
    end do
  end do 
!$OMP END PARALLEL DO
      
!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(jj,j,i,lci,lcj)
  do jj = 1,sibdim(2)
    do j = 1,dimlen_lai(2)
      do i = 1,dimlen_lai(1)
        lci = lcmap(i,j,1)
        lcj = lcmap(i,j,2)
        if ( lcj==jj ) then
          if ( .not.found_missing .or. all( laiin(i,j,1:dimlen_lai(3))/=missing_real ) ) then
            lailocal(lci,lcj,1:dimlen_lai(3)) = lailocal(lci,lcj,1:dimlen_lai(3)) + laiin(i,j,1:dimlen_lai(3))
            countlocal(lci,lcj) = countlocal(lci,lcj) + 1
          end if  
        end if  
      end do
      if ( mod(j,10)==0 .or. j==dimlen_lai(2) ) then
        write(6,*) "User LAI ",j,"/",dimlen_lai(2)
      end if
    end do
  end do
!$OMP END PARALLEL DO
  
  ! replace dataout with non-trival input data
  if ( month==0 ) then
    do iveg = 1,class_num
      do imonth = 1,12
        where ( countlocal(:,:)>0 )
          dataout(:,:,class_num+(iveg-1)*12+imonth) = lailocal(:,:,imonth)/real(countlocal(:,:))
        end where
      end do
    end do  
  else if ( dimlen_lai(3)==12 ) then
    do iveg = class_num+1,num
      where ( countlocal(:,:)>0 )
        dataout(:,:,iveg) = lailocal(:,:,month)/real(countlocal(:,:))
      end where
    end do  
  else
    do iveg = class_num+1,num
      where ( countlocal(:,:)>0 )
        dataout(:,:,iveg) = lailocal(:,:,1)/real(countlocal(:,:))
      end where
    end do  
  end if
  deallocate( laiin, lonin_lai, latin_lai )
  deallocate( lcmap )
  ierr = nf_close(ncidlai)
  
end if

write(6,*) "Finished reading user defined datasets"

return
end subroutine modifylanddata
