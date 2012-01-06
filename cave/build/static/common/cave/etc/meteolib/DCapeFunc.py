##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    17 Feb 2010     #4502         jelkins        Initial Creation.

from numpy import log
from numpy import power as pow
from numpy import sqrt
from numpy import log
from numpy import zeros
from MeteoLibTools import MeteoLibTools as MeteoLib
from CPointer import CPointer
from Temp_of_te import temp_of_te

meteoLib = MeteoLib()
adiabatic_te = meteoLib.adiabatic_te
mytw = meteoLib.mytw

def dcapeFunc(usetv, p_dat, t_dat, td_dat, p0, th0, sh0, max_evap, max_rh):
    """ Calculate Downdraft Convective Available Potential Energy

    ported from dCapeFunc.c
    
    NOTE: until this function is optimized it will be very slow

    @param usetv
                scalar flag to use virtual (1) or plain (0) temperatures 
    @param p_dat
                3D pressure array
    @param t_dat
                3D temperature array
    @param td_dat
                3D dewpoint array
    @param p0
                pressure array
    @param th0
                array of potential temperature
    @param sh0
                array of specific humidity
    @param max_evap
                scalar indicating the maximum amount of liquid water available
                to evaporate into the parcel as it descends
    @param max_rh
                scalar indicating the desired maximum relative humidity
    @return
                an array of dcape_dat
    """
    
    # We input theta and specific humidity for surface conditions because these
    # are things that can be arithemitically averaged for a mixed layer.
    # In order for a dcape to be calculated, there must be positive bouyancy
    # somewhere in the column based on the input surface conditions, neglecting
    # any cap.  Sinking parcel starts from the minimum thetaE level that is above
    # the condensation pressure and below 400mb and the highest positive
    # rising parcel bouyancy.  max_evap is the limit to how much water can
    # be evaporated into the parcel as it decends, in Kg/Kg.  max_rh is
    # the desired RH (%) as the parcel reaches the surface.  Will initially
    # evaporate up to one-third of max_evap into the sinking parcel at the
    # start, afterward attempting to trend the RH toward max_rh at the ground.
    # If usetv=1, buoyancy is done with virtual temp, if usetv=0 with temp.
    
    p_dat_pptr = CPointer(p_dat)
    t_dat_pptr = CPointer(t_dat)
    td_dat_pptr = CPointer(td_dat)
    p0_ptr = CPointer(p0)
    th0_ptr = CPointer(th0)
    sh0_ptr = CPointer(sh0)
    
    # determine the data dimensions
    dataShape = t_dat.shape
    
    nx = dataShape[2] if len(dataShape) > 2 else None
    ny = dataShape[1] if len(dataShape) > 1 else None
    nz = dataShape[0]
    
    # flatten all input arrays
    p_dat.resize((nz, nx * ny,))
    t_dat.resize((nz, nx * ny,))
    td_dat.resize((nz, nx * ny,))
    p0.resize((p0.size,))
    th0.resize((th0.size,))
    sh0.resize((sh0.size,))
    
    mnx = nx # we don't worry about subgrids, so mnx and nx will never be different
    
    #dd = mnx - nx # since we don't subgrid we don't need this variable
                   # any logic that uses dd from the original code has been ommited
    nn = mnx * ny
    n2 = nx * ny
    n3 = n2 * nz
    nzm = nz - 1
    nxm = nx - 1

    # return variables
    dcape_dat_ptr = CPointer(zeros(n2, p_dat.dtype))
    
    # These pointers point to our dynamic storarge.
    tvp_st_ptr = CPointer(zeros(n3, p_dat.dtype))
    tv0_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    tvc_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    tec_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    pc_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    pm_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    tm_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    tdm_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    wm_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    rhm_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    qm_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    
    # Pointer to output data and end of loop pointer
    e_ptr = CPointer()
    dcape_ptr = CPointer()
    
    # Working pointers inside our loops
    pp_ptr = CPointer()
    tt_ptr = CPointer()
    tv_ptr = CPointer()
    tvp_ptr = CPointer()
    tve_ptr = CPointer()
    qq_ptr = CPointer()
    pc_ptr = CPointer()
    tec_ptr = CPointer()
    tvc_ptr = CPointer()
    td3_ptr = CPointer()
    pm_ptr = CPointer()
    tm_ptr = CPointer()
    tdm_ptr = CPointer()
    wm_ptr = CPointer()
    rhm_ptr = CPointer()
    pp1_ptr = CPointer()
    tvp1_ptr = CPointer()
    tve1_ptr = CPointer()
    pp0_ptr = CPointer()
    
    t0 = None
    td = None
    b = None
    up = None
    dn = None
    dlnp = None
    qd = None
    qw = None
    qs = None
    thve = None
    eee = None
    pb = None
    pr = None
    rhmx = None
    k = None
    i = None
    c0 = 26.66082
    c1 = 0.0091379024
    c2 = 6106.396
    c_1 = 223.1986
    c_2 = 0.0182758048
    kapa = 0.286
    kapa_1 = 3.498257
    
    # Calculate the ascending parcel start equivalent temp, virtual temp,
    # and press at LCL, and the initial virtual temperature.  Initialize
    # pm and wm, which now will be pressure at and min environmetal
    # virtual theta E.
    pm_ptr.copy(pm_st_ptr)
    wm_ptr.copy(wm_st_ptr)
    pp0_ptr.copy(p0_ptr)
    tt_ptr.copy(th0_ptr)
    qq_ptr.copy(sh0_ptr)
    tec_ptr.copy(tec_st_ptr)
    tvc_ptr.copy(tvc_st_ptr)
    pc_ptr.copy(pc_st_ptr)
    tv_ptr.copy(tv0_st_ptr)
    e_ptr.copy(pp0_ptr)
    e_ptr += nn
    
    # The following while loop is a pythonish way of writing the equivalent c for loop
    firstIteration = True    
    while (True):
        if not (firstIteration):
            pp0_ptr += 1
            tt_ptr += 1
            qq_ptr += 1
            pc_ptr += 1
            tec_ptr += 1
            tvc_ptr += 1
            tv_ptr += 1
            pm_ptr += 1
            wm_ptr += 1
        firstIteration = False
        if not (pp0_ptr < e_ptr):
            break
        
        pm_ptr[0] = wm_ptr[0] = 1e37
        
        if (pp0_ptr[0] > 1e36 or tt_ptr[0] > 1e36 or qq_ptr[0] > 1e36):
            tec_ptr[0] = tvc_ptr[0] = pc_ptr[0] = 1e37
            continue
        
        t0 = tt_ptr[0] * pow(pp0_ptr[0] / 1000, kapa)
        tv_ptr[0] = t0 * (1 + usetv * 0.000608 * qq_ptr[0])
        b = c0 - log(pp0_ptr[0] / (622. / qq_ptr[0] + 0.378))
        td = (b - sqrt(b * b - c_1)) / c_2
        td -= (t0 - td) * (- 0.37329638 + 41.178204 / t0 + 0.0015945203 * td)
        pc_ptr[0] = pp0_ptr[0] * pow(td / t0, kapa_1)
        tec_ptr[0] = adiabatic_te(td, pc_ptr[0])
        tvc_ptr[0] = td * (1 + usetv * 0.000608 * qq_ptr[0])
    
    # Now calculate the virtual temperature of the accending parcel at the
    # pressures in the input data.
    tvp_ptr.copy(tvp_st_ptr)
    for k in range(0, nz):
        pp_ptr.copy(CPointer(p_dat_pptr[k]))
        tec_ptr.copy(tec_st_ptr)
        tvc_ptr.copy(tvc_st_ptr)
        pc_ptr.copy(pc_st_ptr)
        e_ptr.copy(pp_ptr)
        e_ptr += nn
        
        firstIteration = True    
        while (True):
            if not (firstIteration):
                pp_ptr += 1
                pc_ptr += 1
                tec_ptr += 1
                tvc_ptr += 1
                tvp_ptr += 1
            firstIteration = False
            if not (pp_ptr < e_ptr):
                break
            
            if (pc_ptr[0] > 1e36 or tec_ptr[0] > 1e36 or tvc_ptr[0] > 1e36 or pp_ptr[0] > 1e36):
                tvp_ptr[0] = 1e37
                continue
            
            if (pp_ptr[0] >= pc_ptr[0]):
                tvp_ptr[0] = tvc_ptr[0] * pow(pp_ptr[0] / pc_ptr[0], kapa)
                continue
            
            t0 = tec_ptr[0] * pow(pp_ptr[0] / pc_ptr[0], kapa)
            t0 = temp_of_te(t0, pp_ptr[0])
            tvp_ptr[0] = t0 * pp_ptr[0] / (pp_ptr[0] - usetv * exp(25.687958917 - c1 * t0 - c2 / t0))
            
    # Calculate environment virtual temp, where we force the environment
    # to be no cooler than dry adiabatic from the ascending parcel start.
    # Find pressure of min environmetal virtual theta E above condensation
    # pressure...record temperature and dewpoint there.  Since we do not
    # need the accending parcel temps to complete the dcape calc, we
    # will put the environmental virtual temp into the that storage.
    tm_ptr.copy(tm_st_ptr)
    tdm_ptr.copy(tdm_st_ptr)
    tvp_ptr.copy(tvp_st_ptr)
    tvp_ptr += (n3 + n2)
    for k in range(nzm, - 1, - 1):
        tvp_ptr -= (2 * n2)
        pp_ptr.copy(CPointer(p_dat_pptr[k]))
        tt_ptr.copy(CPointer(t_dat_pptr[k]))
        td3_ptr.copy(CPointer(td_dat_pptr[k]))
        wm_ptr.copy(wm_st_ptr)
        tm_ptr.copy(tm_st_ptr)
        tdm_ptr.copy(tdm_st_ptr)
        pm_ptr.copy(pm_st_ptr)
        tvc_ptr.copy(tvc_st_ptr)
        pc_ptr.copy(pc_st_ptr)
        e_ptr.copy(pp_ptr)
        e_ptr += nn
        
        firstIteration = True    
        while (True):
            if not (firstIteration):
                pp_ptr += 1
                tt_ptr += 1
                td3_ptr += 1
                tvp_ptr += 1
                tvc_ptr += 1
                pc_ptr += 1
                pm_ptr += 1
                tm_ptr += 1
                tdm_ptr += 1
                wm_ptr += 1
            firstIteration = False
            if not (pp_ptr < e_ptr):
                break
            
            if (tvc_ptr[0] > 1e36 or pc_ptr[0] > 1e36 or pp_ptr[0] > 1e36 or tvp_ptr[0] > 1e36 or tt_ptr[0] > 1e36 or td3_ptr[0] > 1e36):
                tvp_ptr[0] = 1e37
                continue
            
            t0 = tt_ptr[0]
            eee = exp(26.186004814 - c1 * td3_ptr[0] - c2 / td3_ptr[0])
            qd = eee / (pp_ptr[0] - 0.60771703 * eee)
            eee = (1 + usetv * 0.608 * qd)
            thve = t0 * eee
            pr = pow(pp_ptr[0] / pc_ptr[0], kapa)
            
            if (thve < tvc_ptr[0] * pr):
                thve = tvc_ptr[0] * pr
                t0 = thve / eee
                
            if (tvp_ptr[0] <= thve and wm_ptr[0] > 1e36 or pp_ptr[0] > pc_ptr[0] and pm_ptr[0] >= 400):
                if (pm_ptr[0] > 1e36 and pp_ptr[0] < pc_ptr[0]):
                    pm_ptr[0] = pc_ptr[0]
                tvp_ptr[0] = thve
                continue

            tvp_ptr[0] = thve;
            thve = (thve + 2529 * qd) * pow(1000 / pp_ptr[0], kapa)
            if (thve > wm_ptr[0] and pm_ptr[0] >= 400):
                continue
            wm_ptr[0] = thve
            pm_ptr[0] = pp_ptr[0]
            tm_ptr[0] = t0
            tdm_ptr[0] = td3_ptr[0]
            
    # Here we will reuse our condensation level storage for
    # the level above current.  This loop performs the actual dcape
    # calculation.
    rhm_ptr.copy(rhm_st_ptr)
    qq_ptr.copy(qm_st_ptr)
    tve_ptr.copy(tvp_st_ptr)
    tve_ptr += (n3 + n2)
    for k in range(nzm, - 1, - 1):
        tve_ptr -= (2 * n2)
        pp_ptr.copy(CPointer(p_dat_pptr[k]))
        tvp1_ptr.copy(tec_st_ptr)
        tve1_ptr.copy(tvc_st_ptr)
        pp1_ptr.copy(pc_st_ptr)
        wm_ptr.copy(wm_st_ptr)
        tm_ptr.copy(tm_st_ptr)
        tdm_ptr.copy(tdm_st_ptr)
        rhm_ptr.copy(rhm_st_ptr)
        qq_ptr.copy(qm_st_ptr)
        pm_ptr.copy(pm_st_ptr)
        pp0_ptr.copy(p0_ptr)
        tv_ptr.copy(tv0_st_ptr)
        dcape_ptr.copy(dcape_dat_ptr)
        e_ptr.copy(pp_ptr)
        e_ptr += nn
        
        firstIteration = True    
        while (True):
            if not (firstIteration):
                pp_ptr += 1
                pp1_ptr += 1
                tve_ptr += 1
                tve1_ptr += 1
                tvp1_ptr += 1
                pp0_ptr += 1
                tv_ptr += 1
                pm_ptr += 1
                wm_ptr += 1
                tm_ptr += 1
                tdm_ptr += 1
                rhm_ptr += 1
                qq_ptr += 1
                dcape_ptr += 1
            firstIteration = False
            if not (pp_ptr < e_ptr):
                break
        
            if (k == nzm):
                dcape_ptr[0] = pp1_ptr[0] = tvp1_ptr[0] = tve1_ptr[0] = 1e37
            if (pm_ptr[0] > 1e36 or pp0_ptr[0] > 1e36 or tv_ptr[0] > 1e36):
                continue
            elif (pp1_ptr[0] > 1e36):
                pass
            elif (pp1_ptr[0] >= pp0_ptr[0]):
                continue
            elif (tve1_ptr[0] > 1e36):
                pass
            elif (pp_ptr[0] > 1e36 or tve_ptr[0] > 1e36):
                continue
            elif (pp_ptr[0] <= pm_ptr[0]):
                pass
            elif (wm_ptr[0] > 1e36):
                dcape_ptr[0] = 0
            else:
                # Now we finally have the data we need for calculating
                # the dcape contribution for this layer.  If we have not
                # made any dcape calculations to this point, initialize
                # the decent parcel.
                if (dcape_ptr[0] > 1e36):
                    dcape_ptr[0] = 0
                    eee = exp(26.186004814 - c1 * tdm_ptr[0] - c2 / tdm_ptr[0])
                    qd = eee / (pm_ptr[0] - 0.60771703 * eee)
                    qw = qd + max_evap / 3
                    t0 = tm_ptr[0] - 2529 * max_evap / 3
                    eee = exp(26.186004814 - c1 * t0 - c2 / t0)
                    qs = eee / (pm_ptr[0] - 0.60771703 * eee)
                    if (qs >= qw):
                        wm_ptr[0] = max_evap - max_evap / 3
                        tm_ptr[0] = t0
                        rhm_ptr[0] = qw / qs
                        b = c0 - log(qw * pm_ptr[0] / (0.622 - 0.378 * qw))
                        tdm_ptr[0] = (b - sqrt(b * b - c_1)) / c_2
                    else:
                        tm_ptr[0] = tdm_ptr[0] = mytw(tm_ptr[0], tdm_ptr[0], pm_ptr[0])
                        rhm_ptr[0] = 1.0
                        eee = exp(26.186004814 - c1 * tm_ptr[0] - c2 / tm_ptr[0])
                        qw = eee / (pm_ptr[0] - 0.60771703 * eee)
                        wm_ptr[0] = max_evap - (qw - qd)
                    
                    qq_ptr[0] = qw
                    tvp1_ptr[0] = tm_ptr[0] * (1 + usetv * 0.608 * qw)
                    pp1_ptr[0] = pm_ptr[0]
                    
                # Deal with reaching the surface, add in top of layer part.
                if (pp_ptr[0] > pp0_ptr[0]):
                    pb = pp0_ptr[0]
                    dlnp = log(pb / pp1_ptr[0])
                    thve = tv_ptr[0]
                else:
                    pb = pp_ptr[0]
                    dlnp = log(pb / pp1_ptr[0])
                    thve = tve_ptr[0]

                up = -dlnp * 287* 0.5 * (tvp1_ptr[0]-tve1_ptr[0])
                if (up<-dcape_ptr[0]):
                    dcape_ptr[0] = 0
                else:
                    dcape_ptr[0] += up
                
                # Deal with letting parcel fall to pb 
                pr = pow(pb/pp1_ptr[0],kapa)
                if (wm_ptr[0]<=0):
                    tvp1_ptr[0] *= pr
                else:
                    rhmx = rhm_ptr[0]+(pb-pp1_ptr[0])*(max_rh-rhm_ptr[0])/(pp0_ptr[0]-pp1_ptr[0])
                    t0 = tm_ptr[0]*pr
                    eee = exp(26.186004814-c1*t0-c2/t0)
                    qs = eee/(pb-0.60771703*eee)
                    if (qq_ptr[0]/qs>rhmx):
                        tm_ptr[0] = t0
                        b = c0-log( qq_ptr[0]*pb/(0.622-0.378*qq_ptr[0]) )
                        tdm_ptr[0] = (b-sqrt(b*b-c_1))/c_2;
                        tvp1_ptr[0] *= pr
                        rhm_ptr[0] = qq_ptr[0]/qs
                    else:
                        qd = (rhmx*qs-qq_ptr[0])/sqrt(1000* (rhmx*qs+qq_ptr[0]) )
                        if (qd>wm_ptr[0]):
                            qd = wm_ptr[0]
                        qw = qq_ptr[0] + qd
                        td = t0 - 2529*wm_ptr[0]
                        eee = exp(26.186004814-c1*td-c2/td)
                        qs = eee/(pb-0.60771703*eee)
                        if (qs>=qw):
                            tm_ptr[0] = td
                            rhm_ptr[0] = qw/qs
                            b = c0-log( qw*pb/(0.622-0.378*qw) )
                            tdm_ptr[0] = (b-sqrt(b*b-c_1))/c_2
                        else:
                            b = c0-log( qq_ptr[0]*pb/(0.622-0.378*qq_ptr[0]) )
                            tdm_ptr[0] = (b-sqrt(b*b-c_1))/c_2
                            tm_ptr[0] = tdm_ptr[0] = mytw(t0, tdm_ptr[0], pb)
                            rhm_ptr[0] = 1.0
                            eee = exp(26.186004814-c1*tm_ptr[0]-c2/tm_ptr[0])
                            qw = eee/(pb-0.60771703*eee)
                            qd = qw-qq_ptr[0]
                            
                        wm_ptr[0] -= qd
                        qq_ptr[0] = qw
                        tvp1_ptr[0] = tm_ptr[0]*(1+usetv*0.608*qw)
                        
                # Add contribution of bottom of layer.
                dn = -dlnp*287*0.5*(tvp1_ptr[0]-thve)
                if (dn<-dcape_ptr[0]):
                    dcape_ptr[0] = 0
                else:
                    dcape_ptr[0] += dn
            
            # Make current layer top next layer bottom. 
            tve1_ptr[0] = tve_ptr[0]
            pp1_ptr[0] = pp_ptr[0]
            
    # unallocate our dynamic storage
    # not needed to be done in python since the memory is released when leaving the method
    
    # resize the arrays back to the appropriate shape
    dcape_dat_ptr.target.resize((ny, nx))
    
    return dcape_dat_ptr.target

def test():
    """ Unit Test
    """
