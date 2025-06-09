! ########################################################################################
!>\file mp_pumas_pre.F90
!!

!> This module contains the pre-processing step prior to calling the PUMAS microphysics.
!
!+ IH
! This is a scheme-specific interstitial scheme (module) that prepares certain input fields
! needed by PUMAS ('PUMAS/micro_pumas_ccpp').
!- IH
! ########################################################################################
module mp_pumas_pre

  use machine, only: kind_phys, kind_dbl_prec

!+ IH
  use physcons, only: rair => con_rd
  use wv_sat_methods,  only: svp_water => wv_sat_svp_water, svp_ice => wv_sat_svp_ice
!- IH

  implicit none
  public mp_pumas_pre_init, mp_pumas_pre_run, mp_pumas_pre_finalize


contains


  ! ######################################################################################
  !> \section arg_table_mp_pumas_pre_init Argument Table
  !! \htmlinclude mp_pumas_pre_init.html
  !!
  ! ######################################################################################
  subroutine mp_pumas_pre_init(errmsg, errflg)
    character(len=*), intent(  out) :: errmsg
    integer,          intent(  out) :: errflg

    ! Initialize the CCPP error handling variables
    errmsg = ''
    errflg = 0

  end subroutine mp_pumas_pre_init



  ! ######################################################################################
  !> \section arg_table_mp_pumas_pre_run Argument Table
  !! \htmlinclude mp_pumas_pre_run.html
  !!
  ! ######################################################################################
  subroutine mp_pumas_pre_run(ncol, nlev, nlevp1, ntrac, tgrs, prsl, prsi, qgrs, &
       ntqv, ntcw, ntiw, ntlnc, ntinc, ntrnc, ntsnc, ntrw, ntsw, ntgl, nthl, ntwa, ntgnc, &
       micro_ncol, micro_nlev, micro_nlevp1, micro_dust_nbins, &
       micro_airT, micro_airq, micro_cldliq, micro_cldice, micro_numliq, &
       micro_numice, micro_rainliq, micro_snowice, micro_numrain, micro_numsnow, micro_graupice,&
       micro_numgraup, micro_relvar, micro_accre_enhan, micro_pmid, micro_pdel, micro_pint,     &
       micro_strat_cldfrc, micro_strat_liq_cldfrc, micro_strat_ice_cldfrc, micro_qsatfac,       &
       micro_naai, micro_npccn, micro_rndst, micro_nacon, micro_snowice_tend_external,          &
       micro_numsnow_tend_external, micro_effi_external, micro_frzimm, micro_frzcnt, micro_frzdep, &
!+ IH: Additional required vars
!       errmsg, errflg)
       micro_timestep, do_shoc, cld_shoc, clcn_i, tice_i, rhc_i, errmsg, errflg)

    implicit none
!- IH

    ! Inputs
    integer, intent(in) :: ncol, nlev, nlevp1, ntrac, micro_ncol, micro_nlev, micro_nlevp1, micro_dust_nbins
    integer, intent(in) :: ntqv, ntcw, ntiw, ntlnc, ntinc, ntrnc, ntsnc, ntrw, ntsw, ntgl, nthl, ntwa, ntgnc
!+ IH
    real(kind_phys), dimension(:,:),   intent(in) :: tgrs, prsl, prsi
    real(kind_phys), dimension(:,:,:), intent(in) :: qgrs
!    real(kind_phys), dimension(1:ncol,1:nlev),   intent(in) :: tgrs, prsl
!    real(kind_phys), dimension(1:ncol,1:nlevp1), intent(in) :: prsi
!    real(kind_phys), dimension(1:ncol,1:nlev,1:ntrac), intent(in) :: qgrs
!- IH

!+ IH: Additional required vars
    ! Required input vars
    real(kind_phys), intent(in) :: micro_timestep
    logical, intent(in) :: do_shoc
    real(kind_phys), dimension(:,:),   intent(in) :: rhc_i
    real(kind_phys), dimension(:,:),   intent(in), optional :: cld_shoc, clcn_i
    real(kind_phys), intent(in) :: tice_i

    ! Local vars
    integer :: i, k
!    real(kind_phys) :: tx1, tice_all, esl, esi, deles
!    real(kind_phys), dimension(micro_ncol,micro_nlev) :: tx, tx2
    real(kind_phys) :: tx, tx1, tx2, tice_all, esl, esi, deles, rhoair
!- IH

    ! Outputs
!+ IH
    real(kind_phys), dimension(:,:), intent(out) :: micro_airT, micro_airq, micro_cldliq,&
!    real(kind_phys), dimension(1:micro_ncol,1:micro_nlev), intent(out) ::                &
!         micro_airT, micro_airq, micro_cldliq,                                           &
!- IH
         micro_cldice, micro_numliq,  micro_numice, micro_rainliq, micro_snowice,        &
         micro_numrain, micro_numsnow, micro_graupice, micro_numgraup, micro_relvar,     &
         micro_accre_enhan, micro_pmid, micro_pdel, micro_pint, micro_strat_cldfrc,      &
         micro_strat_liq_cldfrc, micro_strat_ice_cldfrc, micro_qsatfac, micro_naai,      &
         micro_npccn, micro_snowice_tend_external, micro_numsnow_tend_external,          &
         micro_effi_external, micro_frzimm, micro_frzcnt, micro_frzdep
    real(kind_phys), dimension(:,:,:), intent(out) :: micro_rndst, micro_nacon

    ! CCPP error reporting
    character(len=*), intent(  out) :: errmsg
    integer,          intent(  out) :: errflg

    ! Initialize the CCPP error handling variables
    errmsg = ''
    errflg = 0
    
    ! Sub sample input state by n-subcolumns
    micro_airT(:, 1:micro_nlev)     = tgrs(:, 1:micro_nlev)
    micro_airq(:, 1:micro_nlev)     = qgrs(:, 1:micro_nlev, ntqv)
    micro_cldliq(:, 1:micro_nlev)   = qgrs(:, 1:micro_nlev, ntcw)
    micro_cldice(:, 1:micro_nlev)   = qgrs(:, 1:micro_nlev, ntiw)
    micro_rainliq(:, 1:micro_nlev)  = qgrs(:, 1:micro_nlev, ntrw)
    micro_snowice(:, 1:micro_nlev)  = qgrs(:, 1:micro_nlev, ntsw)
    micro_graupice(:, 1:micro_nlev) = qgrs(:, 1:micro_nlev, ntgl)
    micro_numliq(:, 1:micro_nlev)   = qgrs(:, 1:micro_nlev, ntlnc)
    micro_numice(:, 1:micro_nlev)   = qgrs(:, 1:micro_nlev, ntinc)
    micro_numrain(:, 1:micro_nlev)  = qgrs(:, 1:micro_nlev, ntrnc)
    micro_numsnow(:, 1:micro_nlev)  = qgrs(:, 1:micro_nlev, ntsnc)
    micro_numgraup(:, 1:micro_nlev) = qgrs(:, 1:micro_nlev, ntgnc)
    micro_pmid(:, 1:micro_nlev)     = prsl(:, 1:micro_nlev)
    micro_pint(:, 1:micro_nlev)     = prsi(:, 1:micro_nlevp1)

!+++++ IH
! In CAM, sub 'clubb_tend_cam' sets var 'relvar' to 2 when 'deep_scheme'  ==  'CLUBB_SGS' and relvar(i,k) = min(10.,max(0.001,rcm(i,k)**2/qclvar(i,k))) otherwise.
! For MG3 in CCPP, sub 'micro_mg_init' (in micro_mg3_0.F90) reads in var 'mg_qcvar' as var 'qcvar', which is assigned to var 'qcvar3' and later var 'alphar8' and used by sub 'micro_mg_tend3_0' (in m_micro.F90), or var 'relvar' by sub 'micro_mg_tend' (in micro_mg3_0.F90). Var 'mg_qcvar' is set to 1 in the host (scm/src/GFS_typedefs.F90).
! In CCPP, sub 'micro_pumas_ccpp_run' (in micro_pumas_ccpp.F90) reads in var 'micro_relvar_in' as var 'relvar', which is used by sub 'micro_pumas_tend' (in PUMAS/micro_pumas_v1.F90).
! [ Question: Var 'relvar' seems to be a more CLUBB-related field. Is it critical for the performance of PUMAS? ]
! This var accounts for the nonlinear dependence of cloud microphysical process rates on cloud water content in the MG scheme (Guo et al., 2015). According to Bogenschutz et al. (2013), this var can be obtained from CLUBB (if used); otherwise it is set to a constant constrained by a maximum. Bogenschutz et al. (2013) found that setting the maximum to 1, as the original setup of Morrison & Gettelman (2008), helps improve the simulated magnitudes of SWCF.

    ! microphysics relative variance of cloud water
    micro_relvar(:, 1:micro_nlev) = 1._kind_phys ! 0._kind_phys
!----- IH


!+++++ IH
! For MG3 in CCPP, sub 'micro_mg_tend3_0' (in m_micro.F90) directly uses 1 for var 'accre_enhan_i' (which later becomes var 'accre_enhan') by sub 'micro_mg_tend' (in micro_mg3_0.F90).
! In CCPP, sub 'micro_pumas_ccpp_run' (in micro_pumas_ccpp.F90) reads in var 'micro_accre_enhan_in' as var 'accre_enhan', which is used by sub 'micro_pumas_tend' (in PUMAS/micro_pumas_v1.F90).

    ! microphysics accretion enhancement factor
    micro_accre_enhan(:, 1:micro_nlev) = 1._kind_phys ! 0._kind_phys
!----- IH


!+++++ IH
! For MG3 in CCPP, sub 'm_micro_run' reads in and uses var 'ple' to computes var 'pdelr8', which is used in sub 'micro_mg_tend3_0' (in m_micro.F90 => sub 'micro_mg_tend' in micro_mg3_0.F90).
! In CCPP, sub 'micro_pumas_ccpp_run' (in micro_pumas_ccpp.F90) reads in vars 'micro_pdel_in' as var 'pdel', which is used in sub 'micro_pumas_tend' (in PUMAS/micro_pumas_v1.F90). For now we assume vars 'micro_pdel' and 'prsi' share the same 'i' (not necessarily true if subcolumn function actually works).

    ! Pressure thickness
    do i = 1, micro_ncol
      do k = 1, micro_nlev
        micro_pdel(i,k) = prsi(i,k) - prsi(i,k-1)
      end do
    end do
!----- IH


!+++++ IH
! For MG3 in CCPP, sub 'm_micro_pre_run' (in m_micro_pre.F90) reads in 'cld_shoc' ('cld_sgs' generated by PBL/SHOC/shoc.F90) and 'clcn' (generated by the requested convection scheme; e.g. 'cnvc' from CONV/SAMF/samfdeepcnv.F90) as vars 'cld_frc_MG'/'clcn', which are then used by sub 'm_micro_run' (in m_micro.F90) as vars 'CLLS_io'/'CLCN_i' for determining vars 'CLLS'/'CLCN', which help compute vars 'cldfr8', 'liqcldfr8', and 'icecldfr8' that are used by sub 'micro_mg_tend3_0' (in m_micro.F90 => sub 'micro_mg_tend' in micro_mg3_0.F90).
! In CCPP, sub 'micro_pumas_ccpp_run' (in micro_pumas_ccpp.F90) reads in vars 'micro_strat_cldfrc_in'/'micro_strat_liq_cldfrc_in'/'micro_strat_ice_cldfrc_in' as 'strat_cldfrc'/'strat_liq_cldfrc'/'strat_ice_cldfrc', which are then used as vars 'cldn'/'liqcldf'/'liqcldf' for determining vars 'cldm'/'lcldm'/icldm when 'microp_uniform' = .false. in sub 'micro_pumas_tend' (in PUMAS/micro_pumas_v1.F90).
! [ Question: Var 'cld_shoc' is more likely a CLUBB-related field. Is it critical for the performance of PUMAS? ]

    ! microphysics stratiform cloud area fraction
!    micro_strat_cldfrc(:, 1:micro_nlev) = 0.0_kind_phys

    ! microphysics stratiform cloud liquid area fraction
!    micro_strat_liq_cldfrc(:, 1:micro_nlev) = 0.0_kind_phys

    ! microphysics stratiform cloud ice area fraction
!    micro_strat_ice_cldfrc(:, 1:micro_nlev) = 0.0_kind_phys

! The code below is adapted from a part near the end of sub 'm_micro_run'
!    tice_all = tice_i - 40._kind_phys
    tice_all = 243.15_kind_phys
!    tx = 0._kind_phys
!    tx1 = 0._kind_phys
!    tx2 = 0._kind_phys
    do i = 1, micro_ncol
      do k = 1, micro_nlev
        ! Determining tx1 - adapted from m_micro_pre.F90 & m_micro.F90
        if ( do_shoc ) then
          tx1 = min( cld_shoc(i,k) + clcn_i(i,k), 1._kind_phys)
        else
          tx1 = min( clcn_i(i,k), 1._kind_phys)
        end if
        ! Determing tx and tx2 for total, liquid, and ice cloud fractions
        if ( tx1 .gt. 0._kind_phys ) then
          tx = min(max(tx1, 1.e-5_kind_phys), 1._kind_phys)
        else
          tx = 0._kind_phys
        end if
        micro_strat_cldfrc(i,k) = tx
!        if ( tgrs(i,k) .gt. tice_i ) then
        if ( tgrs(i,k) .gt. 273.15_kind_phys ) then
          micro_strat_liq_cldfrc(i,k) = tx
          micro_strat_ice_cldfrc(i,k) = 0._kind_phys
        else if ( tgrs(i,k) .le. tice_all ) then
          micro_strat_liq_cldfrc(i,k) = 0._kind_phys
          micro_strat_ice_cldfrc(i,k) = tx
        else
!          tx2 = tx * (tice_i - tgrs(i,k)) / (tice_i - tice_all)
          tx2 = tx * (273.15_kind_phys - tgrs(i,k)) / 40._kind_phys
          micro_strat_ice_cldfrc(i,k) = tx2
          micro_strat_liq_cldfrc(i,k) = tx - tx2
        end if
      end do
    end do
!----- IH


!+++++ IH 
! For MG3 in CCPP, sub 'm_micro_run' reads in var 'rhc' from Interstitials/UFS_SCM_NEPTUNE/GFS_suite_interstitial_3.F90 as var 'rhc_i', which eventually becomes var 'rhr8' and is used by sub 'micro_mg_tend3_0' (in m_micro.F90 => sub 'micro_mg_tend' in micro_mg3_0.F90) as var 'qsatfac'.
! In CCPP, sub 'micro_pumas_ccpp_run' (in micro_pumas_ccpp.F90) reads in var 'micro_qsatfac_in' as 'qsatfac', which is then assigned to var 'qsfm' when 'microp_uniform' = .false. in sub 'micro_pumas_tend' (in PUMAS/micro_pumas_v1.F90).

    ! microphysics subgrid cloud water saturation scaling factor
    micro_qsatfac(:, 1:micro_nlev) = rhc_i(:, 1:micro_nlev) ! 0._kind_phys
!----- IH


!+++++ IH
! In CAM, sub 'micro_pumas_cam_tend' (in micro_pumas_cam.F90) reads in vars 'tnd_qsnow'/'tnd_nsnow'/'re_ice', which are then used in sub 'micro_pumas_tend' (in [PUMAS submodule]/micro_pumas_v1.F90). These vars are determined "externally" in CARMA, and are actually treated/formulated only in certain cases, such as 'cirrus_dust'. In most cases, they are only announced yet not determined anyhow.
! In CCPP, sub 'micro_pumas_ccpp_run' (in micro_pumas_ccpp.F90) reads in vars 'micro_snowice_tend_external_in'/'micro_numsnow_tend_external_in'/'micro_effi_external_in' as 'snowice_tend_external'/'numsnow_tend_external'/'effi_external', which are then used as vars 'tnd_qsnow'/'tnd_nsnow'/'re_ice' for computing vars 'prci'/'nprci'/'effi' when 'do_cldice' = .false. in sub 'micro_pumas_tend' (in PUMAS/micro_pumas_v1.F90).

! Without further info and given the fact that 'do_cldice' will be set to true in most cases, I currently set vars 'micro_snowice_tend_external' and 'micro_numsnow_tend_external'to 0. For 'micro_effi_external', I set it to 2.5e-6 so that effi(i,k) = re_ice(i,k)*1.e6_r8 is equal to effi(i,k) when 'do_cldice' = .true. and 'dumi(i,k)' < 'qsmall' (in PUMAS/micro_pumas_v1.F90).

    ! microphysics tendency of snow mixing ratio wrt moist air and condensed water from external microphysics
    micro_snowice_tend_external(:, 1:micro_nlev) = 0._kind_phys

    ! microphysics tendency of mass number concentration of snow wrt moist air and condensed water from external microphysics
    micro_numsnow_tend_external(:, 1:micro_nlev) = 0._kind_phys

    ! microphysics effective radius of stratiform cloud ice particle from external microphysics
    micro_effi_external(:, 1:micro_nlev) = 2.5e-5_kind_phys ! 0._kind_phys
!----- IH


!+++++ IH
! In CAM:
! Sub 'tphysbc' (in physpkg.F90) sequentially calls subs 'microp_aero_run' (in microp_aero.F90) and 'microp_driver_tend' (in microp_driver.F90).
! * If 'use_hetfrz_classnuc' is set to false, sub 'microp_aero_run' calls sub 'nucleate_ice_cam_calc' (in nucleate_ice_cam.F90), which calls sub 'nucleati' (in nucleate_ice.F90) to update var 'naai'. In sub 'nucleati', var 'nimey', or IN tendency associated with Meyers et al. (1992), is added to var 'nuci', i.e. var 'naai' of sub 'nucleate_ice_cam_calc'.
! * If 'use_hetfrz_classnuc' is set to true, sub 'microp_aero_run' calls sub 'hetfrz_classnuc_cam_calc' (in hetfrz_classnuc_cam.F90), which calls sub 'hetfrz_classnuc_calc' (in hetfrz_classnuc.F90). This is an update of CAM7/PUMAS, which uses sub 'hetfrz_classnuc_run' (based on Hoose and M\"{o}hler, 2012) to replace the empirical ice nucleation (deposition) as a function of temperature for mixed phase obtained from sub 'nucleate_ice_cam_calc' (based on Meyers et al., 1992). This is done by zeroing the contribution of sub 'nucleate_ice_cam_calc' to var 'naai' if 'use_hetfrz_classnuc' is set to true. The ice nucleation source from Hoose and M\"{o}hler (2012) comprises immersion, deposition, and contact freezing terms associated with black carbon (BC) and mineral dust (i.e. vars 'frzbcimm', 'frzduimm', 'frzbcdep', 'frzdudep', 'frzbccnt', and 'frzducnt').
! Sub 'microp_driver_tend' calls sub 'micro_pumas_cam_tend' (in micro_pumas_cam.F90), which reads vars 'frzimm', 'frzcnt', and 'frzdep' and feeds them to sub 'micro_pumas_tend' (in [PUMAS submodule]/micro_pumas_v1.F90).
!
! In the current CCPP's PUMAS:
! If 'use_hetfrz_classnuc' is set to true, then vars 'micro_frzimm', 'micro_frzcnt', and 'micro_frzdep' are read by sub
! 'micro_pumas_tend' as vars 'frzimm', 'frzcnt', and 'frzdep', which are later passed to vars 'nnuccc', 'nnucct', and 'nnudep'.
! These vars are involved in the conservation of IN. Relatedly, var 'micro_naai_in' is read by sub 'micro_pumas_tend' as var 'naai',
! which is later passed to var 'nnuccd' that also plays a role in the conservation of IN. Note the unit: vars 'micro_frzimm',
! 'micro_frzcnt', and 'micro_frzdep' have the unit of cm-3 (? cm-3 s-1?), while var 'micro_naai' has the unit of kg-1 s-1. When
!  transferring to var 'nnuccc', the unit is converted: nnuccc = frzimm*1e6/rho (cm-3 s-1 * cm3 m-3 * m3 kg-1 = kg-1 s-1 ). If I
!  assign 'micro_naai' here, I just have to convert the unit from cm-3 s-1 to kg-1 s-1 here. The original formula of Meyers et al.
!  (1992) has the unit of L-1.

    ! microphysics tendency of cloud liquid droplet number concentration due to immersion freezing
    micro_frzimm(:, 1:micro_nlev) = 0._kind_phys

    ! microphysics tendency of cloud liquid droplet number concentration due to contact freezing
    micro_frzcnt(:, 1:micro_nlev) = 0._kind_phys

    ! microphysics tendency of cloud ice number concentration due to deposition nucleation
    micro_frzdep(:, 1:micro_nlev) = 0._kind_phys


!    micro_naai(:, 1:micro_nlev) = 0._kind_phys
    ! (ad-hoc) deposition/condensation nucleation in mixed clouds (-37 C < T < 0 C) following Meyers et al. (1992)
    do i = 1, micro_ncol
      do k = 1, micro_nlev
        if ( tgrs(i,k) .lt. 273.15_kind_phys .and. tgrs(i,k) .gt. 236.15_kind_phys .and. qgrs(i, k, ntcw) .gt. 1.e-12_kind_phys ) then
          esl = svp_water(tgrs(i,k))
          esi = svp_ice(tgrs(i,k))
          deles = (esl - esi)
          rhoair = prsl(i,k)/(rair*tgrs(i,k))
          micro_naai(i,k) = 1.e3_kind_phys * exp( 12.96_kind_phys*deles/esi - 0.639_kind_phys ) / rhoair / micro_timestep ! kg-1 s-1 
        else
          micro_naai(i,k) = 0._kind_phys
        end if
      end do
    end do
!----- IH
    
  end subroutine mp_pumas_pre_run



  ! ######################################################################################
  !> \section arg_table_mp_pumas_pre_finalize Argument Table
  !! \htmlinclude mp_pumas_pre_finalize.html
  !!
  ! ######################################################################################
  subroutine mp_pumas_pre_finalize(errmsg, errflg)
    character(len=*), intent(  out) :: errmsg
    integer,          intent(  out) :: errflg

    ! Initialize the CCPP error handling variables
    errmsg = ''
    errflg = 0

  end subroutine mp_pumas_pre_finalize



end module mp_pumas_pre
