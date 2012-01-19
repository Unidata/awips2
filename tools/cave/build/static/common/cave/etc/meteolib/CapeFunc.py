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
#    12 Feb 2010     #4502         jelkins        Initial Creation.
#    17 ........     .....         .......        Combine capeFuncTop logic

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

def capeFunc(usetv, p_dat, tve_dat, p0, th0, sh0, ptop=None):
    """ Calculate Convective Available Potential Energy (CAPE) and Convective Inhibition (Cin)

    This function has been ported from the c capeFunc(Top), but has not yet been optimized
    and is very slow compared to the native function.
    
    All inputs, except for usetv, are assumed to be numpy arrays 

    @param usetv
                1 :: use virtual temperatures
                0 :: use plain temperatures
    @param p_dat
                pressure
    @param tve_dat
                temperature, if usetv == 1, then virtual temp
    @param p0
                0 pressure
    @param th0
                0 temperature
    @param sh0
                0 specific humidity
    @param ptop
                upper termination pressure
                 
    @return a tuple with (cape,cin)
    
    """
    p_dat_ptr = CPointer(p_dat)
    tve_dat_ptr = CPointer(tve_dat)
    p0_ptr = CPointer(p0)
    th0_ptr = CPointer(th0)
    sh0_ptr = CPointer(sh0)
    ptop_ptr = CPointer(ptop)
    
    has_ptop = ptop != None
    
    # determine the data dimensions
    dataShape = tve_dat.shape
    
    nx = dataShape[2] if len(dataShape) > 2 else None
    ny = dataShape[1] if len(dataShape) > 1 else None
    nz = dataShape[0]
    
    # flatten all input arrays
    p_dat.resize((nz, nx * ny,))
    tve_dat.resize((nz, nx * ny,))
    p0.resize((p0.size,))
    th0.resize((th0.size,))
    sh0.resize((sh0.size,))
    
    mnx = nx # we don't worry about subgrids, so mnx and nx will never be different
    
    #dd = mnx - nx # since we don't subgrid we don't need this variable
                   # any logic that uses dd from the original code has been ommited
    nn = mnx * ny
    n2 = nx * ny
    n3 = n2 * nz
    nxm = nx - 1
    
    # return variables
    cape_dat_ptr = CPointer(zeros(n2, p_dat.dtype))
    cin_dat_ptr = CPointer(zeros(n2, p_dat.dtype))
    
    # These pointers point to our dynamic storarge.
    tvp_st_ptr = CPointer(zeros(n3, p_dat.dtype))
    tec_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    tvc_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    pc_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    pp1_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    
    # Initialize md and pmd, which will be pressure of and max Te delta.
    pmd_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    md_st_ptr = CPointer(zeros(n2, p_dat.dtype))
    
    # Pointer to output data and end of loop pointer
    eptr_ptr = CPointer()
    cap_ptr = CPointer()
    cin_ptr = CPointer()
    
    # Working pointers inside our loops
    pp_ptr = CPointer()
    tt_ptr = CPointer()
    tve_ptr = CPointer()
    tvp_ptr = CPointer()
    qq_ptr = CPointer() 
    pc_ptr = CPointer()
    tec_ptr = CPointer()
    tvc_ptr = CPointer()
    pmd_ptr = CPointer()
    md_ptr = CPointer() 
    pp1_ptr = CPointer()     
    tvp1_ptr = CPointer()
    neg_ptr = CPointer()
    pos_ptr = CPointer()
    pp0_ptr = CPointer()
    pfin_ptr = CPointer()
    
    t0 = None
    td = None
    tdc = None
    b = None
    up = None
    dn = None
    dlnp = None
    k = None
    i = None
    nzm = None
    c0 = 26.66082
    c1 = 0.0091379024
    c2 = 6106.396
    c_1 = 223.1986
    c_2 = 0.0182758048
    kapa = 0.286
    kapa_1 = 3.498257
    
    # Calculate the parcel equivalent temp, virtual temp, and press at LCL.
    # Make working copy of sfc press, use as press below current 3d pressure.
    pp0_ptr.copy(p0_ptr)
    tt_ptr.copy(th0_ptr)
    qq_ptr.copy(sh0_ptr)
    pfin_ptr.copy(ptop_ptr)
    tec_ptr.copy(tec_st_ptr)
    tvc_ptr.copy(tvc_st_ptr)
    pc_ptr.copy(pc_st_ptr)
    pp1_ptr.copy(pp1_st_ptr)
    eptr_ptr.copy(pp0_ptr)
    eptr_ptr += nn
    
    # The following while loop is a pythonish way of writing the equivalent c for loop
    firstIteration = True    
    while (True):
        if not (firstIteration):
            pp0_ptr += 1
            pp1_ptr += 1
            tt_ptr += 1
            qq_ptr += 1
            pc_ptr += 1
            tec_ptr += 1
            tvc_ptr += 1
            pfin_ptr += 1
        firstIteration = False
        if not (pp0_ptr < eptr_ptr):
            break
        
        pp1_ptr[0] = pp0_ptr[0]
        
        if pp0_ptr[0] > 1e36 or tt_ptr[0] > 1e36 or qq_ptr[0] > 1e36 or qq_ptr[0] < 0.0005 or (has_ptop and pp0_ptr[0] < pfin_ptr[0]):
            tec_ptr[0] = tvc_ptr[0] = pc_ptr[0] = 1e37
            continue
        
        t0 = tt_ptr[0] * pow(pp0_ptr[0] / 1000, kapa)
        b = c0 - log(pp0_ptr[0] / (622. / qq_ptr[0] + 0.378))
        td = (b - sqrt(b * b - c_1)) / c_2
        tdc = td - (t0 - td) * (- 0.37329638 + 41.178204 / t0 + 0.0015945203 * td)
        pc_ptr[0] = pp0_ptr[0] * pow(tdc / t0, kapa_1)
        tec_ptr[0] = adiabatic_te(tdc, pc_ptr[0])
        tvc_ptr[0] = td * (1 + usetv * 0.000608 * qq_ptr[0])
        
    # Now calculate the virtual temperature of the parcel at the pressures
    # in the input data.  Then difference it from the environmental temp,
    # which has been tweaked to not be cooler than dry adiabatic from the
    # parcel start.  Record the level of max parcel difference.
    tvp_ptr.copy(tvp_st_ptr)
    nzm = 0
    for k in range(0, nz):
        pp1_ptr.copy(pp1_st_ptr)
        pfin_ptr.copy(ptop_ptr)
        pp_ptr.copy(CPointer(p_dat_ptr[k]))
        tve_ptr.copy(CPointer(tve_dat_ptr[k]))
        tec_ptr.copy(tec_st_ptr)
        tvc_ptr.copy(tvc_st_ptr)
        pc_ptr.copy(pc_st_ptr)
        md_ptr.copy(md_st_ptr)
        pmd_ptr.copy(pmd_st_ptr)
        eptr_ptr.copy(pp_ptr)
        eptr_ptr += nn
        
        firstIteration = True
        while (True):
            if not (firstIteration):
                pp1_ptr += 1
                pp_ptr += 1
                pc_ptr += 1
                pfin_ptr += 1
                tec_ptr += 1
                tvc_ptr += 1
                tvp_ptr += 1
                tve_ptr += 1
                md_ptr += 1
                pmd_ptr += 1
            firstIteration = False
            if not (pp_ptr < eptr_ptr):
                break
            
            if (pc_ptr[0] > 1e36 or pp_ptr[0] > 1e36 or tve_ptr[0] > 1e36 or (has_ptop and pp1_ptr[0] <= pfin_ptr[0])):
                tvp_ptr[0] = 1e37
                continue
            if has_ptop:
                pp1_ptr[0] = pp_ptr[0]
                nzm = k
            t0 = tvc_ptr[0] * pow(pp_ptr[0] / pc_ptr[0], kapa)
            if (pp_ptr[0] >= pc_ptr[0]):
                tvp_ptr[0] = t0
            else:
                td = tec_ptr[0] * pow(pp_ptr[0] / pc_ptr[0], kapa)
                td = temp_of_te(td, pp_ptr[0])
                tvp_ptr[0] = td
                if (usetv > 0):
                    tvp_ptr[0] *= pp_ptr[0] / (pp_ptr[0] - exp(25.687958917 - c1 * td - c2 / td))
            
            if (tve_ptr[0] < t0):
                tvp_ptr[0] -= t0
            else:
                tvp_ptr[0] -= tve_ptr[0]
            if (pp_ptr[0] > pc_ptr[0] or (has_ptop and pp_ptr[0] < pfin_ptr[0]) or tvp_ptr[0] < md_ptr[0]):
                continue
            md_ptr[0] = tvp_ptr[0]
            pmd_ptr[0] = pp_ptr[0]
    
    if has_ptop:
        nz = nzm + 1
        dlnp = 0
    
    # This loop performs the actual cape and cin calculation. Here we will
    # reuse storage for virt temp, equiv temp, and max delta for prev parcel
    # temp, neg and pos.  neg and pos are pending negative and positive
    # contributions we have not yet added into the cape and cin yet.
    tvp_ptr.copy(tvp_st_ptr)
    for k in range (0, nz):
        pp0_ptr.copy(p0_ptr)
        pc_ptr.copy(pc_st_ptr)
        pmd_ptr.copy(pmd_st_ptr)
        pp1_ptr.copy(pp1_st_ptr)
        pp_ptr.copy(CPointer(p_dat_ptr[k]))
        pfin_ptr.copy(ptop_ptr)
        tvp1_ptr.copy(tvc_st_ptr)
        neg_ptr.copy(tec_st_ptr)
        pos_ptr.copy(md_st_ptr)
        cap_ptr.copy(cape_dat_ptr)
        cin_ptr.copy(cin_dat_ptr)
        eptr_ptr.copy(pp_ptr)
        eptr_ptr += nn
        
        firstIteration = True
        while (True):
            if not firstIteration:
                pp0_ptr += 1
                pc_ptr += 1
                pmd_ptr += 1
                pp1_ptr += 1
                pp_ptr += 1
                pfin_ptr += 1
                tvp1_ptr += 1
                tvp_ptr += 1
                cap_ptr += 1
                cin_ptr += 1
                pos_ptr += 1
                neg_ptr += 1
            firstIteration = False
            if not (pp_ptr < eptr_ptr):
                break
            
            if (k == 0):
                cin_ptr[0] = cap_ptr[0] = 1e37
                pos_ptr[0] = neg_ptr[0] = 0
            elif (pp0_ptr[0] > 1e36):
                continue
            elif (pp1_ptr[0] > 1e36 or tvp1_ptr[0] > 1e36):
                pass
            elif (pp_ptr[0] >= pp1_ptr[0] or tvp_ptr[0] > 1e36):
                continue
            elif (pp_ptr[0] >= pp0_ptr[0]):
                pass
            else:
                # Now we finally have the data we need for calculating
                # the cape/cin contribution for this layer.
                if (cap_ptr[0] > 1e36):
                    cap_ptr[0] = cin_ptr[0] = 0
                if (pmd_ptr[0] == 0):
                    continue # No parcel delta > 0, we're done
                
                # First deal with possibility of bottom lvl being below the
                # initial parcel and/or hitting the top of the computation
                if (has_ptop and pp_ptr[0] < pfin_ptr[0]):
                    b = log(pp1_ptr[0] / pp_ptr[0])
                    dlnp = log(pp1_ptr[0] / pfin_ptr[0])
                    tvp_ptr[0] = tvp1_ptr[0] + (dlnp / b) * (tvp_ptr[0] - tvp1_ptr[0]);
                if (pp1_ptr[0] > pp0_ptr[0]):
                    if (has_ptop and pp_ptr[0] < pfin_ptr[0]):
                        dlnp = log(pp0_ptr[0] / pfin_ptr[0])
                    else:
                        dlnp = log(pp0_ptr[0] / pp_ptr[0])
                    dn = 0
                else:
                    if (not(has_ptop) or (has_ptop and pp_ptr[0] >= pfin_ptr[0])):
                        dlnp = log(pp1_ptr[0] / pp_ptr[0])
                    dn = dlnp * 287 * tvp1_ptr[0]
                
                # Now deal with the fact that not allowing superadiabatic
                # layers means no cape below condensation pressure
                if (pp1_ptr[0] >= pc_ptr[0]):
                    if (dn > 0):
                        dn = 0
                    if (tvp_ptr[0] <= 0):
                        up = dlnp * 287 * tvp_ptr[0]
                    elif (pp_ptr[0] >= pc_ptr[0]):
                        up = 0
                    elif (has_ptop and pp_ptr[0] < pfin_ptr[0]):
                        up = log(pc_ptr[0] / pfin_ptr[0]) * 287 * tvp_ptr[0]
                    else:
                        up = log(pc_ptr[0] / pp_ptr[0]) * 287 * tvp_ptr[0]
                else:
                    up = dlnp * 287 * tvp_ptr[0]
                
                # Deal with where the break point is
                b = 0.5 if up * dn >= 0 else up / (up - dn)
                up *= b
                dn *= (1 - b)
                
                # Now consider this layer's contribution, taking into account
                # transitions between positive and negative acceleration
                if (up == 0 and dn == 0):
                    pass
                elif (up <= 0 and (dn < 0 or dn == 0 and (pp_ptr[0] < pmd_ptr[0] or pos_ptr[0] == 0))):
                    # Continuing deceleration
                    neg_ptr[0] -= up + dn
                elif (up >= 0 and (dn > 0 or dn == 0 and (pp_ptr[0] < pmd_ptr[0] or neg_ptr[0] == 0))):
                    # Continuing upward acceleration
                    pos_ptr[0] += up + dn
                    if (pp_ptr[0] > pmd_ptr[0] and cap_ptr[0] + pos_ptr[0] <= cin_ptr[0] + neg_ptr[0]):
                        pass # no net cape and below max delta
                    elif (pp_ptr[0] > pmd_ptr[0] or cap_ptr[0] == 0):
                        # below max delta or cape uninitialized
                        cap_ptr[0] += pos_ptr[0]
                        cin_ptr[0] += neg_ptr[0]
                        neg_ptr[0] = pos_ptr[0] = 0
                    elif (pos_ptr[0] >= neg_ptr[0]):
                        # cape initialized and net positive contribution
                        cap_ptr[0] += pos_ptr[0] - neg_ptr[0]
                        neg_ptr[0] = pos_ptr[0] = 0
                elif(up > 0 and dn <= 0):
                    # Transition to upward acceleration
                    neg_ptr[0] += - dn
                    if (pp1_ptr[0] <= pmd_ptr[0]):
                        # above max delta, only use net pos contribution
                        pos_ptr[0] += up
                        if (pos_ptr[0] >= neg_ptr[0]):
                            cap_ptr[0] += pos_ptr[0] - neg_ptr[0]
                            neg_ptr[0] = pos_ptr[0] = 0
                    elif (pp_ptr[0] <= pmd_ptr[0]):
                        # straddle max delta, force cape initialization
                        if (cap_ptr[0] == 0):
                            cin_ptr[0] += neg_ptr[0]
                            cap_ptr[0] += pos_ptr[0]
                        elif (neg_ptr[0] > pos_ptr[0]):
                            cin_ptr[0] += neg_ptr[0] - pos_ptr[0]
                        else:
                            cap_ptr[0] += pos_ptr[0] - neg_ptr[0]
                        cap_ptr[0] += up
                        neg_ptr[0] = pos_ptr[0] = 0
                    elif ((cap_ptr[0] + pos_ptr[0] + up) <= (cin_ptr[0] + neg_ptr[0])):
                        # no net cape to this point
                        if (cap_ptr[0] + pos_ptr[0] > 0):
                            # reinitialize if there was cape before
                            cin_ptr[0] -= cap_ptr[0] + pos_ptr[0]
                            pos_ptr[0] = cap_ptr[0] = 0
                        cin_ptr[0] += neg_ptr[0]
                        pos_ptr[0] += up
                        neg_ptr[0] = 0
                    elif (cap_ptr[0] == 0):
                        # initialize cape
                        cap_ptr[0] += pos_ptr[0] + up
                        cin_ptr[0] += neg_ptr[0]
                        neg_ptr[0] = pos_ptr[0] = 0
                    else:
                        # what remains, only use net pos contribution
                        pos_ptr[0] += up
                        if (pos_ptr[0] >= neg_ptr[0]):
                            cap_ptr[0] += pos_ptr[0] - neg_ptr[0]
                            neg_ptr[0] = pos_ptr[0] = 0
                else:
                    # Transition to decceleration
                    pos_ptr[0] += dn
                    if (pp1_ptr[0] <= pmd_ptr[0]):
                        # above max delta, only use net pos contribution
                        if (pos_ptr[0] >= neg_ptr[0]):
                            cap_ptr[0] += pos_ptr[0] - neg_ptr[0]
                            neg_ptr[0] = pos_ptr[0] = 0
                        neg_ptr[0] += - up
                    elif (cap_ptr[0] + pos_ptr[0] <= cin_ptr[0] + neg_ptr[0] - up):
                        # no net cape to this point
                        if (cap_ptr[0] > 0):
                            cin_ptr[0] -= cap_ptr[0] + pos_ptr[0]
                            pos_ptr[0] = cap_ptr[0] = 0
                        cin_ptr[0] += neg_ptr[0] - up
                        pos_ptr[0] = neg_ptr[0] = 0
                    elif (cap_ptr[0] == 0):
                        # initialize cape
                        cap_ptr[0] += pos_ptr[0]
                        cin_ptr[0] += neg_ptr[0] - up
                        neg_ptr[0] = pos_ptr[0] = 0
                    else:
                        # what remains, only use net pos contribution
                        if (pos_ptr[0] >= neg_ptr[0]):
                            cap_ptr[0] += pos_ptr[0] - neg_ptr[0]
                            neg_ptr[0] = pos_ptr[0] = 0
                        neg_ptr[0] += - up
                        
            # Make current layer top next layer bottom
            tvp1_ptr[0] = tvp_ptr[0]
            pp1_ptr[0] = pp_ptr[0]
            
    # unallocate our dynamic storage
    # not needed to be done in python since the memory is released when leaving the method
    
    # resize the arrays back to the appropriate shape
    cape_dat_ptr.target.resize((ny, nx))
    cin_dat_ptr.target.resize((ny, nx))
    
    return cape_dat_ptr.target, cin_dat_ptr.target

def test():
    """ Unit Test
    """
