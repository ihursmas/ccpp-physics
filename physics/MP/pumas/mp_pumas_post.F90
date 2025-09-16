! ########################################################################################
!>\file mp_pumas_post.F90
!!

!> This module contains the post-processing step after calling the PUMAS microphysics.
!
! ########################################################################################
module mp_pumas_post

  use machine, only: kind_phys, kind_dbl_prec
  use micro_pumas_diags, only: proc_rates_type

!+ IH
  use physcons, only: cp => con_cp
!- IH

  implicit none

  public mp_pumas_post_init, mp_pumas_post_run, mp_pumas_post_finalize

contains
  ! ######################################################################################
  !> \section arg_table_mp_pumas_post_init Argument Table
  !! \htmlinclude mp_pumas_post_init.html
  !!
  ! ######################################################################################
  subroutine mp_pumas_post_init(errmsg, errflg)
    character(len=*), intent(  out) :: errmsg
    integer,          intent(  out) :: errflg

    ! Initialize the CCPP error handling variables
    errmsg = ''
    errflg = 0

  end subroutine mp_pumas_post_init

  ! ######################################################################################
  !> \section arg_table_mp_pumas_post_run Argument Table
  !! \htmlinclude mp_pumas_post_run.html
  !!
  ! ######################################################################################
  subroutine mp_pumas_post_run(micro_prect, micro_preci, micro_proc_rates, micro_qcsinksum_rate1ord, &
             micro_airT_tend, micro_airq_tend, micro_cldliq_tend, micro_cldice_tend, &
             micro_numliq_tend, micro_numice_tend, micro_rainliq_tend, micro_snowice_tend,&
             micro_numrain_tend, micro_numsnow_tend, micro_graupice_tend, micro_numgraup_tend,&
             micro_effc, micro_effc_fn, micro_effi, micro_sadice, micro_sadsnow,&
             micro_prec_evap, micro_am_evap_st, micro_prec_prod, micro_cmeice,&
             micro_deffi, micro_pgamrad, micro_lamcrad, micro_snowice_in_prec,&
             micro_scaled_diam_snow, micro_graupice_in_prec, micro_numgraup_vol_in_prec,&
             micro_scaled_diam_graup, micro_lflx, micro_iflx, micro_gflx,&
             micro_rflx, micro_sflx, micro_rainliq_in_prec, micro_reff_rain,&
             micro_reff_snow, micro_reff_grau, micro_numrain_vol_in_prec,&
             micro_numsnow_vol_in_prec, micro_refl, micro_arefl, micro_areflz, &
             micro_frefl, micro_csrfl, micro_acsrfl, micro_fcsrfl, micro_refl10cm,&
             micro_reflz10cm, micro_rercld, micro_ncai, micro_ncal, micro_rainliq,&
             micro_snowice, micro_numrain_vol, micro_numsnow_vol, micro_diam_rain,&
             micro_diam_snow, micro_graupice, micro_numgraup_vol, micro_diam_graup,&
             micro_freq_graup, micro_freq_snow, micro_freq_rain, micro_frac_ice, &
             micro_frac_cldliq_tend, micro_rain_evap, &
!+ IH
             micro_ncol,micro_nlev, micro_timestep, tgrs, tgrs_save, qgrs, qgrs_save, &
             ntqv, ntcw, ntiw, ntlnc, ntinc, ntrnc, ntsnc, ntrw, ntsw, ntgl, nthl, ntgnc, &
!- IH
             errmsg, errflg)

    ! Inputs
    real(kind_phys), dimension(:),   intent(in) :: micro_prect, micro_preci
    real(kind_phys), dimension(:,:), intent(in) :: &
         micro_qcsinksum_rate1ord, &
         micro_airT_tend, &
         micro_airq_tend, &
         micro_cldliq_tend, &
         micro_cldice_tend, &
         micro_numliq_tend, &
         micro_numice_tend, &
         micro_rainliq_tend, &
         micro_snowice_tend, &
         micro_numrain_tend, &
         micro_numsnow_tend, &
         micro_graupice_tend, &
         micro_numgraup_tend, &
         micro_effc, &
         micro_effc_fn, &
         micro_effi, &
         micro_sadice, &
         micro_sadsnow, &
         micro_prec_evap, &
         micro_am_evap_st, &
         micro_prec_prod, &
         micro_cmeice, &
         micro_deffi, &
         micro_pgamrad, &
         micro_lamcrad, &
         micro_snowice_in_prec, &
         micro_scaled_diam_snow, &
         micro_graupice_in_prec, &
         micro_numgraup_vol_in_prec, &
         micro_scaled_diam_graup, &
         micro_lflx, &
         micro_iflx, &
         micro_gflx, &
         micro_rflx, &
         micro_sflx, &
         micro_rainliq_in_prec, &
         micro_reff_rain, &
         micro_reff_snow, &
         micro_reff_grau, &
         micro_numrain_vol_in_prec, &
         micro_numsnow_vol_in_prec, &
         micro_refl, &
         micro_arefl, &
         micro_areflz, &
         micro_frefl, &
         micro_csrfl, &
         micro_acsrfl, &
         micro_fcsrfl, &
         micro_refl10cm, &
         micro_reflz10cm, &
         micro_rercld, &
         micro_ncai, &
         micro_ncal, &
         micro_rainliq, &
         micro_snowice, &
         micro_numrain_vol, &
         micro_numsnow_vol, &
         micro_diam_rain, &
         micro_diam_snow, &
         micro_graupice, &
         micro_numgraup_vol, &
         micro_diam_graup, &
         micro_freq_graup, &
         micro_freq_snow, &
         micro_freq_rain, &
         micro_frac_ice, &
         micro_frac_cldliq_tend, &
         micro_rain_evap
    
    type(proc_rates_type), intent(inout) ::  micro_proc_rates

!+ IH
    integer, intent(in) :: micro_ncol, micro_nlev
    integer, intent(in) :: ntqv, ntcw, ntiw, ntlnc, ntinc, ntrnc, ntsnc, ntrw, ntsw, ntgl, nthl, ntgnc
    real(kind_phys), intent(in) :: micro_timestep
    real(kind_phys), dimension(:,:), intent(in)    :: tgrs_save
    real(kind_phys), dimension(:,:), intent(out)   :: tgrs
    real(kind_phys), dimension(:,:,:), intent(in)  :: qgrs_save
    real(kind_phys), dimension(:,:,:), intent(out) :: qgrs

    ! Local vars
    integer :: i, k, rk
!- IH
   
    ! CCPP error handling
    character(len=*), intent(  out) :: errmsg
    integer,          intent(  out) :: errflg

    ! Initialize the CCPP error handling variables
    errmsg = ''
    errflg = 0

!+ IH
!    tgrs(:,1:micro_nlev)       = tgrs_save(:,1:micro_nlev)       + micro_airT_tend(:,1:micro_nlev)/cp *micro_timestep
!    qgrs(:,1:micro_nlev,ntqv)  = qgrs_save(:,1:micro_nlev,ntqv)  + micro_airq_tend(:,1:micro_nlev)    *micro_timestep
!    qgrs(:,1:micro_nlev,ntcw)  = qgrs_save(:,1:micro_nlev,ntcw)  + micro_cldliq_tend(:,1:micro_nlev)  *micro_timestep
!    qgrs(:,1:micro_nlev,ntiw)  = qgrs_save(:,1:micro_nlev,ntiw)  + micro_cldice_tend(:,1:micro_nlev)  *micro_timestep
!    qgrs(:,1:micro_nlev,ntrw)  = qgrs_save(:,1:micro_nlev,ntrw)  + micro_rainliq_tend(:,1:micro_nlev) *micro_timestep
!    qgrs(:,1:micro_nlev,ntsw)  = qgrs_save(:,1:micro_nlev,ntsw)  + micro_snowice_tend(:,1:micro_nlev) *micro_timestep
!    qgrs(:,1:micro_nlev,ntgl)  = qgrs_save(:,1:micro_nlev,ntgl)  + micro_graupice_tend(:,1:micro_nlev)*micro_timestep
!    qgrs(:,1:micro_nlev,ntlnc) = qgrs_save(:,1:micro_nlev,ntlnc) + micro_numliq_tend(:,1:micro_nlev)  *micro_timestep
!    qgrs(:,1:micro_nlev,ntinc) = qgrs_save(:,1:micro_nlev,ntinc) + micro_numice_tend(:,1:micro_nlev)  *micro_timestep
!    qgrs(:,1:micro_nlev,ntrnc) = qgrs_save(:,1:micro_nlev,ntrnc) + micro_numrain_tend(:,1:micro_nlev) *micro_timestep
!    qgrs(:,1:micro_nlev,ntsnc) = qgrs_save(:,1:micro_nlev,ntsnc) + micro_numsnow_tend(:,1:micro_nlev) *micro_timestep
!    qgrs(:,1:micro_nlev,ntgnc) = qgrs_save(:,1:micro_nlev,ntgnc) + micro_numgraup_tend(:,1:micro_nlev)*micro_timestep

    do i = 1, micro_ncol
      do k = 1, micro_nlev
! Reversing the vertical index for output fields.
!    micro_x(i,1) = x(i,micro_nlev): x at the level of micro_nlev;
!    micro_x(i,micro_nlev) = x(i,1): x at the surface.
        rk = micro_nlev - k + 1      ! k = 1, rk = micro_nlev; k = 2, rk = micro_nlev-1; ...; k = micro_nlev-1, rk = 2; k = micro_nlev, rk = 1
        tgrs(i,k)       = tgrs_save(i,k)       + micro_airT_tend(i,rk)/cp *micro_timestep
        qgrs(i,k,ntqv)  = qgrs_save(i,k,ntqv)  + micro_airq_tend(i,rk)    *micro_timestep
        qgrs(i,k,ntcw)  = qgrs_save(i,k,ntcw)  + micro_cldliq_tend(i,rk)  *micro_timestep
        qgrs(i,k,ntiw)  = qgrs_save(i,k,ntiw)  + micro_cldice_tend(i,rk)  *micro_timestep
        qgrs(i,k,ntrw)  = qgrs_save(i,k,ntrw)  + micro_rainliq_tend(i,rk) *micro_timestep
        qgrs(i,k,ntsw)  = qgrs_save(i,k,ntsw)  + micro_snowice_tend(i,rk) *micro_timestep
        qgrs(i,k,ntgl)  = qgrs_save(i,k,ntgl)  + micro_graupice_tend(i,rk)*micro_timestep
        qgrs(i,k,ntlnc) = qgrs_save(i,k,ntlnc) + micro_numliq_tend(i,rk)  *micro_timestep
        qgrs(i,k,ntinc) = qgrs_save(i,k,ntinc) + micro_numice_tend(i,rk)  *micro_timestep
        qgrs(i,k,ntrnc) = qgrs_save(i,k,ntrnc) + micro_numrain_tend(i,rk) *micro_timestep
        qgrs(i,k,ntsnc) = qgrs_save(i,k,ntsnc) + micro_numsnow_tend(i,rk) *micro_timestep
        qgrs(i,k,ntgnc) = qgrs_save(i,k,ntgnc) + micro_numgraup_tend(i,rk)*micro_timestep
      end do
    end do

!+++ test
!    tgrs(:,1:micro_nlev) = tgrs_save(:,1:micro_nlev) + 0.01_kind_phys
!    qgrs(:,1:micro_nlev,ntqv)  = qgrs_save(:,1:micro_nlev,ntqv) + 0.0001_kind_phys
!    qgrs(:,1:micro_nlev,ntcw)  = qgrs_save(:,1:micro_nlev,ntcw) + 0.00001_kind_phys
!    qgrs(:,1:micro_nlev,ntiw)  = qgrs_save(:,1:micro_nlev,ntiw) + 0.00001_kind_phys
!    qgrs(:,1:micro_nlev,ntlnc) = qgrs_save(:,1:micro_nlev,ntlnc) + 0.0000001_kind_phys
!    qgrs(:,1:micro_nlev,ntinc) = qgrs_save(:,1:micro_nlev,ntinc) + 0.0000001_kind_phys
!    qgrs(:,1:micro_nlev,ntrw)  = qgrs_save(:,1:micro_nlev,ntrw) + 0.000001_kind_phys
!    qgrs(:,1:micro_nlev,ntsw)  = qgrs_save(:,1:micro_nlev,ntsw) + 0.000001_kind_phys
!    qgrs(:,1:micro_nlev,ntrnc) = qgrs_save(:,1:micro_nlev,ntrnc) + 0.0000001_kind_phys
!    qgrs(:,1:micro_nlev,ntsnc) = qgrs_save(:,1:micro_nlev,ntsnc) + 0.0000001_kind_phys
!    qgrs(:,1:micro_nlev,ntgl)  = qgrs_save(:,1:micro_nlev,ntgl) + 0.000001_kind_phys
!    qgrs(:,1:micro_nlev,ntgnc) = qgrs_save(:,1:micro_nlev,ntgnc) + 0.0000001_kind_phys
!--- test
!- IH


  end subroutine mp_pumas_post_run

  ! ######################################################################################
  !> \section arg_table_mp_pumas_post_finalize Argument Table
  !! \htmlinclude mp_pumas_post_finalize.html
  !!
  ! ######################################################################################
  subroutine mp_pumas_post_finalize(errmsg, errflg)
    character(len=*), intent(  out) :: errmsg
    integer,          intent(  out) :: errflg

    ! Initialize the CCPP error handling variables
    errmsg = ''
    errflg = 0

  end subroutine mp_pumas_post_finalize

end module mp_pumas_post
