/*
	File:		setup_funcs.c
	Date:		12/14/1994
	Author:		Dale Shelton
	
	Purpose:	
	
*/


#include <Xm/Xm.h>
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "CoopComms.h"
#include "CoopRecip.h"
#include "CoopSpons.h"
#include "DamTypes.h"
#include "DcpOwner.h"
#include "GageMaint.h"
#include "GageOwner.h"
#include "GageType.h"
#include "Hsa.h"
#include "Network.h"
#include "ResOwner.h"
#include "Rfc.h"
#include "TelmOwner.h"
#include "TelmPayor.h"
#include "TelmType.h"
#include "Wfo.h"
#include "setup_cbs.h"
#include "setup_funcs.h"
#include "setup_tree.h"
#include "hybase.h"


void	comms_load(void)
{
	CoopComms	*comms, 
			*commsPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((comms = GetCoopComms(" ORDER BY comm ")) != NULL)
	{
		count = ListCount(&comms->list);
		xmStr = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		commsPtr = (CoopComms *) ListFirst(&comms->list);
		for (i = 0; commsPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(commsPtr->comm);
			commsPtr = (CoopComms *) ListNext(&commsPtr->node);
		}
		
		FreeCoopComms(comms);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	recip_load(void)
{
	CoopRecip	*cr, 
			*crPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((cr = GetCoopRecip(" ORDER BY recip ")) != NULL)
	{
		count = ListCount(&cr->list);
		xmStr = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		crPtr = (CoopRecip *) ListFirst(&cr->list);
		for (i = 0; crPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(crPtr->recip);
			crPtr = (CoopRecip *) ListNext(&crPtr->node);
		}
		
		FreeCoopRecip(cr);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	spons_load(void)
{
	CoopSpons	*cs, 
			*csPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((cs = GetCoopSpons(" ORDER BY spons ")) != NULL)
	{
		count = ListCount(&cs->list);
		xmStr = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		csPtr = (CoopSpons *) ListFirst(&cs->list);
		for (i = 0; csPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(csPtr->spons);
			csPtr = (CoopSpons *) ListNext(&csPtr->node);
		}
		
		FreeCoopSpons(cs);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	downer_load(void)
{
	DcpOwner	*dcp, 
			*dcpPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((dcp = GetDcpOwner(" ORDER BY owner ")) != NULL)
	{
		count  = ListCount(&dcp->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		dcpPtr = (DcpOwner *) ListFirst(&dcp->list);
		for (i = 0; dcpPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(dcpPtr->owner);
			dcpPtr = (DcpOwner *) ListNext(&dcpPtr->node);
		}
		
		FreeDcpOwner(dcp);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	gmaint_load(void)
{
	GageMaint	*gm, 
			*gmPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((gm = GetGageMaint(" ORDER BY maint ")) != NULL)
	{
		count  = ListCount(&gm->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		gmPtr = (GageMaint *) ListFirst(&gm->list);
		for (i = 0; gmPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(gmPtr->maint);
			gmPtr = (GageMaint *) ListNext(&gmPtr->node);
		}
		
		FreeGageMaint(gm);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	gowner_load(void)
{
	GageOwner	*go, 
			*goPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((go = GetGageOwner(" ORDER BY owner ")) != NULL)
	{
		count  = ListCount(&go->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		goPtr = (GageOwner *) ListFirst(&go->list);
		for (i = 0; goPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(goPtr->owner);
			goPtr = (GageOwner *) ListNext(&goPtr->node);
		}
		
		FreeGageOwner(go);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	gtype_load(void)
{
	GageType	*gt, 
			*gtPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((gt = GetGageType(" ORDER BY type ")) != NULL)
	{
		count  = ListCount(&gt->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		gtPtr = (GageType *) ListFirst(&gt->list);
		for (i = 0; gtPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(gtPtr->type);
			gtPtr = (GageType *) ListNext(&gtPtr->node);
		}
		
		FreeGageType(gt);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	hsa_load(void)
{
	Hsa		*hsa, 
			*hsaPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((hsa = GetHsa(" ORDER BY hsa ")) != NULL)
	{
		count  = ListCount(&hsa->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		hsaPtr = (Hsa *) ListFirst(&hsa->list);
		for (i = 0; hsaPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(hsaPtr->hsa);
			hsaPtr = (Hsa *) ListNext(&hsaPtr->node);
		}
		FreeHsa(hsa);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	net_load(void)
{
	Network		*net, 
			*netPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((net = GetNetwork(" ORDER BY network ")) != NULL)
	{
		count  = ListCount(&net->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		netPtr = (Network *) ListFirst(&net->list);
		for (i = 0; netPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(netPtr->network);
			netPtr = (Network *) ListNext(&netPtr->node);
		}
		
		FreeNetwork(net);
	}
	
	setup_load(&xmStr, count);
	
	return;
}



void	dtype_load(void)
{
	DamTypes	*dt, 
			*dtPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((dt = GetDamTypes(" ORDER BY type ")) != NULL)
	{
		count = ListCount(&dt->list);
		xmStr = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		dtPtr = (DamTypes *) ListFirst(&dt->list);
		for (i = 0; dtPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(dtPtr->type);
			dtPtr = (DamTypes *) ListNext(&dtPtr->node);
		}
		
		FreeDamTypes(dt);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	rowner_load(void)
{
	ResOwner	*res,
			*resPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;


	if ((res = GetResOwner(" ORDER BY owner ")) != NULL)
	{
		count  = ListCount(&res->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		resPtr = (ResOwner *) ListFirst(&res->list);
		for (i = 0; resPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(resPtr->owner);
			resPtr = (ResOwner *) ListNext(&resPtr->node);
		}
		
		FreeResOwner(res);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	rfc_load(void)
{
	Rfc		*rfc, 
			*rfcPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((rfc = GetRfc(" ORDER BY rfc ")) != NULL)
	{
		count  = ListCount(&rfc->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		rfcPtr = (Rfc *) ListFirst(&rfc->list);
		for (i = 0; rfcPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(rfcPtr->rfc);
			rfcPtr   = (Rfc *) ListNext(&rfcPtr->node);
		}
		
		FreeRfc(rfc);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	towner_load(void)
{
	TelmOwner	*tm, 
			*tmPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((tm = GetTelmOwner(" ORDER BY owner ")) != NULL)
	{
		count  = ListCount(&tm->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		tmPtr = (TelmOwner *) ListFirst(&tm->list);
		for (i = 0; tmPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(tmPtr->owner);
			tmPtr   = (TelmOwner *) ListNext(&tmPtr->node);
		}
		
		FreeTelmOwner(tm);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	tpayor_load(void)
{
	TelmPayor	*tp, 
			*tpPtr;
	XmStringTable	xmStr;
	int		count = 0,
			i;
			
	
	if ((tp = GetTelmPayor(" ORDER BY payor ")) != NULL)
	{
		count  = ListCount(&tp->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		tpPtr = (TelmPayor *) ListFirst(&tp->list);
		for (i = 0; tpPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(tpPtr->payor);
			tpPtr   = (TelmPayor *) ListNext(&tpPtr->node);
		}
		
		FreeTelmPayor(tp);
	}
	
	setup_load(&xmStr, count);
	
	return;
}


void	ttype_load(void)
{
	TelmType	*tt, 
			*ttPtr;
	XmStringTable	xmStr = NULL ;
	int		count = 0,
			i;
			
	
	if ((tt = GetTelmType(" ORDER BY type ")) != NULL)
	{
		count  = ListCount(&tt->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		ttPtr = (TelmType *) ListFirst(&tt->list);
		for (i = 0; ttPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(ttPtr->type);
			ttPtr   = (TelmType *) ListNext(&ttPtr->node);
		}
		
		FreeTelmType(tt);
	}
	
	setup_load(&xmStr, count);
		
	return;
}


void	wfo_load(void)
{
	Wfo		*tt, 
			*ttPtr;
	XmStringTable	xmStr = NULL ;
	int		count = 0,
			i;
			
	
	if ((tt = GetWfo(" ORDER BY wfo ")) != NULL)
	{
		count  = ListCount(&tt->list);
		xmStr  = (XmStringTable) XtMalloc(count * sizeof(XmString *));
		ttPtr = (Wfo *) ListFirst(&tt->list);
		for (i = 0; ttPtr; i++)
		{
			xmStr[i] = XmStringCreateSimple(ttPtr->wfo);
			ttPtr   = (Wfo *) ListNext(&ttPtr->node);
		}
		
		FreeWfo(tt);
	}
	
	setup_load(&xmStr, count);
	
	return;
}




