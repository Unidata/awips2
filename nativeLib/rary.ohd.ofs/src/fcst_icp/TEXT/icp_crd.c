/*****************************************************************************
 * icpcrd - called from mccards, gives overall parametric values
 CALL ICPCRD(IMO, IYR, LMO, LYR, IDARUN, LDARUN, METRIC, P, MP)
 *****************************************************************************/

#define MAIN_ICP_C 1
#include <icpcommon/icp_out.h>

void icpcrd(imo, iyr, lmo, lyr, idarun, ldarun, metric, p, mp)
	int *imo, *iyr, *lmo, *lyr, *idarun, *ldarun, *metric, *mp;char *p; {
	int ascii_out = 0;
	float tot_obs = 0.0;
	int ifirst = 1;
	int first_wyp = 1;
	char text_out_sac[] = "icp_sac_out.txt";
	char text_out_crd[] = "icp_crd_out.txt";
	char text_out_wyp[] = "icp_wyp_out.txt";
	char text_out_sno17[] = "icp_sno17_out.txt";
	char text_out_plotts[] = "icp_plotts_out.txt";
	char tok_intrfc_dir[] = "mcp3_icp_iface";
	char seven_2[] = " %7.2f", seven_3[] = " %7.3f";
	int mons[2][13] = { { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
			{ 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 } };

	char user_name[9], op_name[9], redef_name[9], line_three[5];
	int i, j, k, opr_num, nxtp, lcl_mp, out_array[8];
	Joint jnflt;

	toklen = strlen(tok_intrfc_dir);
	get_apps_defaults(tok_intrfc_dir, &toklen, envvar, &varlen);
	if (envvar)
		strcpy(intrfc_dir, envvar);
	else
		strcpy(intrfc_dir, " ");

	/*
	 if (!strcpy(intrfc_dir,get_apps_defaults("mcp3_icp_iface")))
	 strcpy(intrfc_dir," ");
	 else printf("in icpcrd else leg: mcp3_icp_face is |%s|\n", intrfc_dir);
	 */
	strcat(strcat(strcpy(work_file, intrfc_dir), "/"), text_out_crd);
	if (!(txt_out = fopen(work_file, "w")))
		printf("in icpcrd: cannot open %s for writing!!\n\n", work_file);

	strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "sac_scr.txt");
	if (!(sac_scr = fopen(work_file, "w"))) {
		printf("in icpcrd: cannot open %s for write!!\n\n", work_file);
		return;
	}
	strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "wyp_scr.txt");
	if (!(wyp_scr = fopen(work_file, "w"))) {
		printf("in icpcrd: cannot open %s for write!!\n\n", work_file);
		return;
	}
	strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "sno17_scr.txt");
	if (!(sno17_scr = fopen(work_file, "w"))) {
		printf("in icpcrd: cannot open %s for write!!\n\n", work_file);
		return;
	}
	strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "plotts_scr.txt");
	if (!(plotts_scr = fopen(work_file, "w"))) {
		printf("in icpcrd: cannot open %s for write!!\n\n", work_file);
		return;
	}

	strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "icp_sufx.txt");
	if (!(icp_incrd = fopen(work_file, "r"))) {
		printf("in icpcrd: cannot open %s!!\n\n", work_file);
		return;
	}
	fgets(file_suffix, 6, icp_incrd);
	if (chrptr = strchr(file_suffix, '\n'))
		*chrptr = '\0';
	else if (chrptr = strchr(file_suffix, ' '))
		*chrptr = '\0';
	else
		file_suffix[4] = '\0';
	fgets(shed_name, 9, icp_incrd);
	if (chrptr = strchr(shed_name, '\n'))
		*chrptr = '\0';
	else if (chrptr = strchr(shed_name, ' '))
		*chrptr = '\0';
	else
		shed_name[9] = '\0';
	if (fgets(line_three, 4, icp_incrd)) {
		if (chrptr = strchr(line_three, '\n'))
			*chrptr = '\0';
		else if (chrptr = strchr(line_three, ' '))
			*chrptr = '\0';
		else
			line_three[3] = '\0';
		ascii_out = atoi(line_three);
	} else
		ascii_out = 0;
	fclose(icp_incrd);
	num_sac = num_wyp = num_sno17 = num_plotts = 0;
	file_name_wyp = file_name_sac = file_name_sno17 = file_name_plotts
			= (char **) NULL;
	wyp_data = sac_data = sno17_data = plotts_data = (float **) NULL;
	strcat(strcat(strcat(strcat(strcpy(file_name_card, intrfc_dir), "/"),
			shed_name), ".icp_crd.bin_data.24."), file_suffix);
	if (!(icp_incrd = fopen(file_name_card, "w"))) {
		printf("in icpcrd: cannot open %s!!\n\n", file_name_card);
		return;
	}
	/*
	 printf(" in icpcrd:\n");
	 printf(" imo=%d, iyr=%d, lmo=%d, lyr=%d, num_obs=%d, metric=%d, mp=%d\n",
	 out_array[0] = *imo, out_array[1] = *iyr,
	 out_array[2] = *lmo, out_array[3] = *lyr,
	 (out_array[5] = *ldarun) - (out_array[4] = *idarun) +1,
	 out_array[6]=*metric, lcl_mp=out_array[7]=*mp);
	 */
	fprintf(txt_out, " in icpcrd:\n");
	fprintf(txt_out,
			" imo=%d, iyr=%d, lmo=%d, lyr=%d, num_obs=%d, metric=%d, mp=%d\n",
			out_array[0] = *imo, out_array[1] = *iyr, out_array[2] = *lmo,
			out_array[3] = *lyr, (out_array[5] = *ldarun) - (out_array[4]
					= *idarun) + 1, out_array[6] = *metric, lcl_mp
					= out_array[7] = *mp);
	if (ascii_out)
		fprintf(icp_incrd, " %d %d %d %d %d %d %d %d %d\n", out_array[0],
				out_array[1], out_array[2], out_array[3], out_array[4],
				out_array[5], out_array[6], out_array[7]);
	else
		fwrite((char *) out_array, sizeof(int), 8, icp_incrd), fwrite(p,
				sizeof(float), lcl_mp, icp_incrd);
	/*
	 printf("  now, the p array:\n");
	 printf("  j  num  user_name  redef_name  nxtp\n");
	 printf(" --  ---  ---------  ----------  ----\n");
	 */
	fprintf(txt_out, "  now, the p array:\n");
	fprintf(txt_out, "  j  num  user_name  redef_name  nxtp\n");
	fprintf(txt_out, " --  ---  ---------  ----------  ----\n");

	nxtp = 1;
	j = 0;
	while (nxtp < lcl_mp && opr_num != -1 && j < 200) {
		pin = &p[(nxtp - 1) * 4];
		FLIT(opr_num)
		FLIT(nxtp)
		for (k = 0; k < 8; k++)
			op_name[k] = tolower(user_name[k] = *pin++);
		op_name[k] = user_name[k] = '\0';
		for (k = 0; k < 8; k++)
			redef_name[k] = *pin++;
		redef_name[k] = '\0';
		/*
		 printf(" %2d  %3d  %s    %s    %d\n",
		 j, opr_num, user_name, redef_name, nxtp);
		 */
		fprintf(txt_out, " %2d  %3d  %s    %s    %d\n", j, opr_num, user_name,
				redef_name, nxtp);
		if ((opr_num == 1 && ++num_sac) || (opr_num == 17 && ++num_wyp)
				|| (opr_num == 19 && ++num_sno17) || (opr_num == 18
				&& ++num_plotts)) {
			for (k = 0; user_name[k] != ' ' && k < 8; k++)
				;
			op_name[k] = user_name[k] = '\0';
			if (opr_num == 1)
				fwrite(op_name, 1, 9, sac_scr);
			else if (opr_num == 17)
				fwrite(op_name, 1, 9, wyp_scr);
			else if (opr_num == 19)
				fwrite(op_name, 1, 9, sno17_scr);
			else if (opr_num == 18)
				fwrite(op_name, 1, 9, plotts_scr);
			if (ascii_out)
				fprintf(icp_incrd, " %9.9s %5.5s %.4d\n", user_name,
						file_suffix, opr_num);
			else
				fwrite(user_name, 1, 9, icp_incrd), fwrite(file_suffix, 1, 5,
						icp_incrd), fwrite((char *) &opr_num, sizeof(int), 1,
						icp_incrd);
		}
		j++;
	}
	fprintf(txt_out, " j= %d, opr_num= %d\n", j, opr_num);
	/* printf(" j= %d, opr_num= %d\n", j, opr_num); */
	strcpy(user_name, "no_more"), opr_num = 0;
	for (k = 0; user_name[k] != ' ' && k < 8; k++)
		;
	user_name[k] = '\0';
	if (ascii_out)
		fprintf(icp_incrd, " %9.9s %5.5s %.4d\n", user_name, file_suffix,
				opr_num);
	else
		fwrite(user_name, 1, 9, icp_incrd), fwrite(file_suffix, 1, 5, icp_incrd), fwrite(
				(char *) &opr_num, sizeof(int), 1, icp_incrd);
	fclose(icp_incrd);
	fclose(sac_scr);
	fclose(wyp_scr);
	fclose(sno17_scr);
	fclose(plotts_scr);
	/*
	 printf("  num_sac %d, num_wyp %d, num_sno17 %d, num_plotts %d\n",
	 num_sac, num_wyp, num_sno17, num_plotts);
	 */
	fprintf(txt_out, "  num_sac %d, num_wyp %d, num_sno17 %d, num_plotts %d\n",
			num_sac, num_wyp, num_sno17, num_plotts);
	fclose(txt_out);
	/* --------------------------
	 in the following, the malloc-ing of 15-20 items for the various _data
	 arrays is type dependent.  in fact sac uses 0-13, and 14 is used to
	 keep the month count; wyp uses none and 0 is used; sno17 uses 0-14 for
	 data, and 15 for the count (thus sno17 allocates 20 items).  hs 7/27/95
	 */
	if (num_sac > 0) {
		file_name_sac = (char **) malloc(sizeof(char *) * num_sac);
		sac_opnames = (char **) malloc(sizeof(char *) * num_sac);
		sac_data = (float **) malloc(sizeof(float *) * num_sac);
		strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "sac_scr.txt");
		if (!(sac_scr = fopen(work_file, "r"))) {
			printf("in icpcrd: cannot open work_file %s for read!!\n\n",
					work_file);
			return;
		}
		for (i = 0; i < num_sac; i++) {
			sac_opnames[i] = (char *) malloc(sizeof(char) * 9);
			sac_data[i] = (float *) malloc(sizeof(float) * SAC_VARS);
			file_name_sac[i] = (char *) malloc(sizeof(char) * 150);
			fread(sac_opnames[i], 1, 9, sac_scr);
			strcat(strcat(strcat(strcat(strcpy(file_name_sac[i], intrfc_dir),
					"/"), sac_opnames[i]), ".sac_sma.bin_data.24."),
					file_suffix);
		}
		fclose(sac_scr);
	}
	if (num_wyp > 0) {
		file_name_wyp = (char **) malloc(sizeof(char *) * num_wyp);
		wyp_opnames = (char **) malloc(sizeof(char *) * num_wyp);
		wyp_data = (float **) malloc(sizeof(float *) * num_wyp);
		strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "wyp_scr.txt");
		if (!(wyp_scr = fopen(work_file, "r"))) {
			printf("in icpcrd: cannot open work_file %s for read!!\n\n",
					work_file);
			return;
		}
		for (i = 0; i < num_wyp; i++) {
			wyp_opnames[i] = (char *) malloc(sizeof(char) * 9);
			wyp_data[i] = (float *) malloc(sizeof(float) * WYP_VARS);
			file_name_wyp[i] = (char *) malloc(sizeof(char) * 150);
			fread(wyp_opnames[i], 1, 9, wyp_scr);
			strcat(strcat(strcat(strcat(strcpy(file_name_wyp[i], intrfc_dir),
					"/"), wyp_opnames[i]), ".wy_plot.bin_data.24."),
					file_suffix);
		}
		fclose(wyp_scr);
	}
	if (num_sno17 > 0) {
		file_name_sno17 = (char **) malloc(sizeof(char *) * num_sno17);
		sno17_opnames = (char **) malloc(sizeof(char *) * num_sno17);
		sno17_data = (float **) malloc(sizeof(float *) * num_sno17);
		strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "sno17_scr.txt");
		if (!(sno17_scr = fopen(work_file, "r"))) {
			printf("in icpcrd: cannot open work_file %s for read!!\n\n",
					work_file);
			return;
		}
		for (i = 0; i < num_sno17; i++) {
			sno17_opnames[i] = (char *) malloc(sizeof(char) * 9);
			sno17_data[i] = (float *) malloc(sizeof(float) * SNO17_VARS);
			file_name_sno17[i] = (char *) malloc(sizeof(char) * 150);
			fread(sno17_opnames[i], 1, 9, sno17_scr);
			strcat(strcat(strcat(strcat(strcpy(file_name_sno17[i], intrfc_dir),
					"/"), sno17_opnames[i]), ".snow_17.bin_data.24."),
					file_suffix);
		}
		fclose(sno17_scr);
	}
	if (num_plotts > 0) {
		file_name_plotts = (char **) malloc(sizeof(char *) * num_plotts);
		plotts_opnames = (char **) malloc(sizeof(char *) * num_plotts);
		plotts_data = (float **) malloc(sizeof(float *) * num_plotts);
		strcat(strcat(strcpy(work_file, intrfc_dir), "/"), "plotts_scr.txt");
		if (!(plotts_scr = fopen(work_file, "r"))) {
			printf("in icpcrd: cannot open work_file %s for read!!\n\n",
					work_file);
			return;
		}
		for (i = 0; i < num_plotts; i++) {
			plotts_opnames[i] = (char *) malloc(sizeof(char) * 9);
			plotts_data[i] = (float *) malloc(sizeof(float) * PLOTTS_VARS);
			file_name_plotts[i] = (char *) malloc(sizeof(char) * 150);
			fread(plotts_opnames[i], 1, 9, plotts_scr);
			strcat(strcat(strcat(strcat(
					strcpy(file_name_plotts[i], intrfc_dir), "/"),
					plotts_opnames[i]), ".plotts.bin_data."), file_suffix);
		}
		fclose(plotts_scr);
	}

	/*  ==============  Statements containing RCS keywords:  */
	{
		static char rcs_id1[] =
				"$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_icp/RCS/icp_crd.c,v $";
		static char rcs_id2[] =
				"$Id: icp_crd.c,v 1.3 1999/04/23 17:21:56 page Exp $";
	}
	/*  ===================================================  */

}
