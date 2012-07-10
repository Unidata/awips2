/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPCacheRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableCellData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableRowData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfigData;

/**
 * FFMP Data generator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2009           dhladky     Initial creation
 * Jan 25, 2012 DR 13839  gzhang	  Use paintTime for QPF
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFMPDataGenerator {
	private FfmpTableConfig tableConfig;

	private static final transient IUFStatusHandler statusHandler = UFStatus
			.getHandler(FFMPDataGenerator.class);

	FFMPConfig ffmpCfg = FFMPConfig.getInstance();

	FFMPTemplates ft = null;

	FFMPResource resource = null;

	FFMPMonitor monitor = null;

	FFMPBasinData qpeBasin = null;

	FFMPBasinData qpfBasin = null;

	FFMPBasinData rateBasin = null;

	HashMap<String, FFMPBasinData> guidBasins = null;

	FFMPBasinData virtualBasin = null;

	FFMPCacheRecord rateRecord = null;

	FFMPCacheRecord qpeRecord = null;

	FFMPCacheRecord qpfRecord = null;

	HashMap<String, FFMPCacheRecord> guidRecords = null;

	FFMPCacheRecord virtualRecord = null;

	FFMPCacheRecord baseRec = null;

	// Date time = null;

	// Date recentTime = null;

	SourceXML primarySource = null;

	FFFGDataMgr dman = null;

	boolean isRate = false;

	long expirationTime = 0l;

	private String[] cwaArr = null;

	private HashMap<String, FFFGForceUtil> forceUtils = null;

	private FfmpTableConfigData ffmpTableCfgData = null;

	public FFMPDataGenerator(FFMPMonitor monitor, FFMPResource resource) {
		this.tableConfig = FfmpTableConfig.getInstance();
		this.resource = resource;
		this.monitor = monitor;
		this.ft = monitor.getTemplates(resource.getSiteKey());
		this.primarySource = resource.getResourceData().getPrimarySourceXML();
		this.isRate = primarySource.isRate();
		this.expirationTime = primarySource.getExpirationMinutes(resource
				.getSiteKey()) * 60 * 1000;
		ffmpTableCfgData = tableConfig
				.getTableConfigData(resource.getSiteKey());
	}

	public FFMPTableData generateFFMPData() throws Exception {
		// You should always have at least a QPE data source
		FFMPTableData tData = null;
		// update the FFFGDataManager
		FFFGDataMgr.getUpdatedInstance();
		tData = new FFMPTableData();

		try {
			FIELDS field = getBaseField();
			if (field != null) {
				if (baseRec != null) {
					FFMPBasinData fbd = null;
					if (resource.centeredAggregationKey != null) {
						fbd = baseRec.getBasinData("ALL");
					} else {
						fbd = baseRec.getBasinData(resource.getHuc());
					}

					if (fbd.getBasins().size() > 0) {
						if ((resource.centeredAggregationKey == null)
								|| resource.getHuc().equals("ALL")) {
							// System.out.println(fbd.getBasins().keySet().size()
							// + " rows in the table");
							for (Long key : fbd.getBasins().keySet()) {
								if (resource.getHuc().equals("ALL")) {
									for (DomainXML domain : resource
											.getDomains()) {

										FFMPBasinMetaData fmdb = ft.getBasin(
												resource.getSiteKey(), key);

										if (fmdb == null) {
											continue;
										}

										if ((domain.getCwa().equals(fmdb
												.getCwa()))
												|| (domain.isPrimary() && fmdb
														.isPrimaryCwa())) {

											setFFMPRow(fbd.get(key), tData,
													false, domain.getCwa());

											if (virtualBasin != null) {
												for (Long id : ft
														.getVirtualGageBasinLookupIds(
																resource.getSiteKey(),
																key)) {
													setFFMPRow(
															virtualBasin
																	.get(id),
															tData, true, domain
																	.getCwa());
												}
											}
										}
									}

								} else {
									/*
									 * make sure at least one basin in the agg
									 * is in the CWA
									 */

									ArrayList<Long> pfafs = ft
											.getAggregatePfafs(key,
													resource.getSiteKey(),
													resource.getHuc());

									boolean isVGB = false;
									if (ft.checkVGBsInAggregate(key,
											resource.getSiteKey(),
											resource.getHuc())) {
										isVGB = true;
									}

									if (pfafs.size() > 0) {

										FFMPBasinMetaData fmdb = ft
												.getBasinInDomains(
														resource.getSiteKey(),
														resource.getDomains(),
														pfafs);

										if (fmdb != null) {
											try {
												setFFMPRow(fbd.get(key), tData,
														isVGB, null);
											} catch (Exception e) {
												e.printStackTrace();
											}
										}
									}
								}
							}
						}
						// show pfafs in aggregation
						else {
							for (Long key : resource
									.getCenteredAggregatePfafs()) {

								FFMPBasinMetaData fmdb = ft.getBasin(
										resource.getSiteKey(), key);

								if (fmdb != null) {
									for (DomainXML domain : resource
											.getDomains()) {

										if ((domain.getCwa().equals(fmdb
												.getCwa()))
												|| (domain.isPrimary() && fmdb
														.isPrimaryCwa())) {

											setFFMPRow(fbd.get(key), tData,
													false, null);
											// virtual basin
											if (virtualBasin != null) {
												for (Long id : ft
														.getVirtualGageBasinLookupIds(
																resource.getSiteKey(),
																key)) {
													setFFMPRow(
															virtualBasin
																	.get(id),
															tData, true, null);
												}
											}
										}
									}
								}
							}
						}

						tData.sortData();
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		return tData;
	}

	private void setFFMPRow(FFMPBasin cBasin, FFMPTableData tData,
			boolean isVGB, String domain) {
		try {
			if (cBasin instanceof FFMPVirtualGageBasin) {
				if (tData.containsPfaf(((FFMPVirtualGageBasin) cBasin).getLid()
						.toString()) == true) {
					return;
				}
			} else {
				if (tData.containsPfaf(cBasin.getPfaf().toString()) == true) {
					return;
				}
			}
		} catch (Exception e) {
			return;
		}

		String displayName = "";
		String mouseOverText = "";

		FFMPTableRowData trd = new FFMPTableRowData(
				ffmpTableCfgData.getTableColumnKeys().length);

		Float guidance = Float.NaN;
		Float qpe = Float.NaN;
		Float rate = Float.NaN;
		Float qpf = Float.NaN;
		FIELDS rowField = FIELDS.NAME;

		if (isVGB) {
			rowField = FIELDS.VIRTUAL;
		}

		if (cBasin instanceof FFMPVirtualGageBasin) {

			rowField = FIELDS.VIRTUAL;

			displayName = ((FFMPVirtualGageBasin) cBasin).getLid();

			if (displayName != null) {

				// in this special case it is actually the LID
				trd.setPfaf(((FFMPVirtualGageBasin) cBasin).getLid());
				FFMPVirtualGageBasinMetaData fvgmbd = ft
						.getVirtualGageBasinMetaData(resource.getSiteKey(),
								((FFMPVirtualGageBasin) cBasin).getLid());
				FFMPBasinMetaData metabasin = ft.getBasin(
						resource.getSiteKey(), fvgmbd.getParentPfaf());
				Long parentBasinPfaf = fvgmbd.getParentPfaf();

				if (fvgmbd != null) {
					mouseOverText = metabasin.getBasinId() + "\n"
							+ fvgmbd.getLid() + "-" + fvgmbd.getName();

					if (!resource.getHuc().equals("ALL")) {
						displayName += "-" + fvgmbd.getName();
					}
				}

				trd.setTableCellData(0, new FFMPTableCellData(rowField,
						displayName, mouseOverText));

				if (!resource.isWorstCase() || resource.getHuc().equals("ALL")
						|| (resource.centeredAggregationKey != null)) {

					if (cBasin.getValues().size() > 0) {
						rate = ((FFMPVirtualGageBasin) cBasin)
								.getValue(resource.getPaintTime().getRefTime());
						trd.setTableCellData(1, new FFMPTableCellData(
								FIELDS.RATE, rate));
					} else {
						trd.setTableCellData(1, new FFMPTableCellData(
								FIELDS.RATE, Float.NaN));
					}
					if (cBasin.getValues().size() > 0) {

						if (resource.getTime() > 0.00) {
							qpe = cBasin.getAccumValue(monitor.getQpeWindow()
									.getAfterTime(), monitor.getQpeWindow()
									.getBeforeTime(), expirationTime, isRate);
						} else {
							qpe = 0.0f;
						}
						trd.setTableCellData(2, new FFMPTableCellData(
								FIELDS.QPE, qpe));

					} else {
						trd.setTableCellData(2, new FFMPTableCellData(
								FIELDS.QPE, Float.NaN));
					}

					if ((qpfBasin != null)
							&& (qpfBasin.get(parentBasinPfaf) != null)) {
						qpf = qpfBasin.get(parentBasinPfaf).getAverageValue(
								monitor.getQpfWindow().getAfterTime(),
								monitor.getQpfWindow().getBeforeTime());
						trd.setTableCellData(3, new FFMPTableCellData(
								FIELDS.QPF, qpf));
					} else {
						trd.setTableCellData(3, new FFMPTableCellData(
								FIELDS.QPF, Float.NaN));
					}

					// run over each guidance type
					int i = 0;

					for (String guidType : guidBasins.keySet()) {
						ArrayList<Long> pfafList = new ArrayList<Long>();
						ArrayList<Long> forcedPfafs = new ArrayList<Long>();
						guidance = Float.NaN;
						boolean forced = false;
						FFFGForceUtil forceUtil = forceUtils.get(guidType);
						FFMPBasinData guidBasin = guidBasins.get(guidType);
						forceUtil.setSliderTime(resource.getTime());

						if ((guidBasin != null)
								&& ((FFMPGuidanceBasin) guidBasin
										.get(parentBasinPfaf) != null)) {
							FFMPGuidanceBasin ffmpGuidBasin = ((FFMPGuidanceBasin) guidBasin
									.get(parentBasinPfaf));

							// If aggregate, get basins within the aggregate
							if (cBasin.getAggregated()) {
								if (domain == null) {
									pfafList = ft.getAggregatePfafs(
											cBasin.getPfaf(),
											resource.getSiteKey(),
											resource.getHuc());
								} else if (!domain.equals("NA")) {
									if (!resource.getHuc().equals("ALL")) {
										pfafList = ft
												.getAggregatePfafsByDomain(
														parentBasinPfaf,
														resource.getSiteKey(),
														domain,
														resource.getHuc());
									}
								} else {
									pfafList = ft.getAggregatePfafsByDomain(
											parentBasinPfaf,
											resource.getSiteKey(), domain,
											resource.getHuc());
									pfafList.add(ft.getAggregatedPfaf(
											cBasin.getPfaf(),
											resource.getSiteKey(),
											resource.getHuc()));
								}
							}

							FFFGDataMgr fdm = FFFGDataMgr.getInstance();

							if (fdm.isForcingConfigured()) {
								FFMPBasin parentBasin = baseRec.getBasinData(
										"ALL").get(parentBasinPfaf);
								forceUtil.calculateForcings(domain, ft,
										parentBasin);
								forcedPfafs = forceUtil.getForcedPfafList();
								forced = forceUtil.isForced();
							}

							if (((forcedPfafs.size() > 1)) && forced) {
								// Recalculate the guidance using the forced
								// value(s)
								guidance = guidRecords
										.get(guidType)
										.getBasinData("ALL")
										.getAverageGuidanceValue(
												pfafList,
												resource.getGuidanceInterpolators()
														.get(guidType),
												guidance,
												forcedPfafs,
												resource.getGuidSourceExpiration());
							} else if (forcedPfafs.size() > 1) {
								guidance = guidRecords
										.get(guidType)
										.getBasinData("ALL")
										.getAverageGuidanceValue(
												pfafList,
												resource.getGuidanceInterpolators()
														.get(guidType),
												Float.NaN,
												forcedPfafs,
												resource.getGuidSourceExpiration());
								forced = true;
							} else if (pfafList.size() > 1) {
								guidance = guidRecords
										.get(guidType)
										.getBasinData("ALL")
										.getAverageGuidanceValue(
												pfafList,
												resource.getGuidanceInterpolators()
														.get(guidType),
												Float.NaN,
												forcedPfafs,
												resource.getGuidSourceExpiration());
							} else {
								guidance = resource.getGuidanceValue(
										ffmpGuidBasin, resource.getPaintTime()
												.getRefTime(), guidType);

								if (guidance < 0.0f) {
									guidance = Float.NaN;
								}
							}

							trd.setTableCellData(i + 4, new FFMPTableCellData(
									FIELDS.GUIDANCE, guidance, forced));
						} else {
							trd.setTableCellData(i + 4, new FFMPTableCellData(
									FIELDS.GUIDANCE, Float.NaN));
						}

						if (!qpe.isNaN() && (guidance > 0.0f)) {
							trd.setTableCellData(
									i + 5,
									new FFMPTableCellData(FIELDS.RATIO,
											FFMPUtils.getRatioValue(qpe,
													guidance)));
							trd.setTableCellData(
									i + 6,
									new FFMPTableCellData(FIELDS.DIFF,
											FFMPUtils.getDiffValue(qpe,
													guidance)));
						} else {
							trd.setTableCellData(i + 5, new FFMPTableCellData(
									FIELDS.RATIO, Float.NaN));
							trd.setTableCellData(i + 6, new FFMPTableCellData(
									FIELDS.DIFF, Float.NaN));
						}

						i += 3;
					}
				} else {
					trd = getMaxValue(trd, cBasin);
				}

				tData.addDataRow(trd);

			}
		} else {
			displayName = getDisplayName(cBasin);

			if (displayName != null) {
				trd.setPfaf(cBasin.getPfaf().toString());
				trd.setTableCellData(0, new FFMPTableCellData(rowField,
						displayName, cBasin.getPfaf().toString() + "\n"
								+ displayName));

				if (!resource.isWorstCase() || resource.getHuc().equals("ALL")
						|| (resource.centeredAggregationKey != null)) {
					if ((rateBasin != null)
							&& (rateBasin.get(cBasin.getPfaf()) != null)) {
						rate = rateBasin.get(cBasin.getPfaf()).getValue(resource.getPaintTime().getRefTime());
						trd.setTableCellData(1, new FFMPTableCellData(
								FIELDS.RATE, rate));
						// System.out.println("rate: "+rate);
					} else {
						trd.setTableCellData(1, new FFMPTableCellData(
								FIELDS.RATE, Float.NaN));
					}
					if ((qpeBasin != null)
							&& (qpeBasin.get(cBasin.getPfaf()) != null)) {
						qpe = qpeBasin.get(cBasin.getPfaf()).getAccumValue(
								monitor.getQpeWindow().getAfterTime(),
								monitor.getQpeWindow().getBeforeTime(),
								expirationTime, isRate);
						trd.setTableCellData(2, new FFMPTableCellData(
								FIELDS.QPE, qpe));
					} else {
						trd.setTableCellData(2, new FFMPTableCellData(
								FIELDS.QPE, Float.NaN));
					}
					if ((qpfBasin != null)
							&& (qpfBasin.get(cBasin.getPfaf()) != null)) {

						qpf = qpfBasin.get(cBasin.getPfaf()).getAverageValue(
								monitor.getQpfWindow().getAfterTime(),
								monitor.getQpfWindow().getBeforeTime());
						// qpf = getQPFValue(false, cBasin.getPfaf(),
						// new ArrayList<Long>());/* DR13839 */
						trd.setTableCellData(3, new FFMPTableCellData(
								FIELDS.QPF, qpf));
						// System.out.println("qpf: "+qpf);
					} else {
						trd.setTableCellData(3, new FFMPTableCellData(
								FIELDS.QPF, Float.NaN));
					}

					// run over each guidance type
					int i = 0;
					for (String guidType : guidBasins.keySet()) {
						ArrayList<Long> pfafList = new ArrayList<Long>();
						ArrayList<Long> forcedPfafs = new ArrayList<Long>();
						guidance = Float.NaN;
						boolean forced = false;
						FFFGForceUtil forceUtil = forceUtils.get(guidType);
						FFMPBasinData guidBasin = guidBasins.get(guidType);
						forceUtil.setSliderTime(resource.getTime());

						if ((guidBasin != null)
								&& ((FFMPGuidanceBasin) guidBasin.get(cBasin
										.getPfaf()) != null)) {
							FFMPGuidanceBasin ffmpGuidBasin = ((FFMPGuidanceBasin) guidBasin
									.get(cBasin.getPfaf()));

							// If aggregate, get basins within the aggregate
							if (cBasin.getAggregated()) {
								if (domain == null) {
									pfafList = ft.getAggregatePfafs(
											cBasin.getPfaf(),
											resource.getSiteKey(),
											resource.getHuc());
								} else if (!domain.equals("NA")) {
									if (!resource.getHuc().equals("ALL")) {
										pfafList = ft
												.getAggregatePfafsByDomain(
														cBasin.getPfaf(),
														resource.getSiteKey(),
														domain,
														resource.getHuc());
									}
								} else {
									pfafList = ft.getAggregatePfafsByDomain(
											cBasin.getPfaf(),
											resource.getSiteKey(), domain,
											resource.getHuc());
									pfafList.add(ft.getAggregatedPfaf(
											cBasin.getPfaf(),
											resource.getSiteKey(),
											resource.getHuc()));
								}
							}

							FFFGDataMgr fdm = FFFGDataMgr.getInstance();

							if (fdm.isForcingConfigured()) {
								forceUtil.calculateForcings(domain, ft, cBasin);
								forcedPfafs = forceUtil.getForcedPfafList();
								forced = forceUtil.isForced();
							}

							if (((forcedPfafs.size() > 1)) && forced) {
								// Recalculate the guidance using the forced
								// value(s)
								guidance = guidRecords
										.get(guidType)
										.getBasinData("ALL")
										.getAverageGuidanceValue(
												pfafList,
												resource.getGuidanceInterpolators()
														.get(guidType),
												guidance,
												forcedPfafs,
												resource.getGuidSourceExpiration());
							} else if (forcedPfafs.size() > 1) {
								guidance = guidRecords
										.get(guidType)
										.getBasinData("ALL")
										.getAverageGuidanceValue(
												pfafList,
												resource.getGuidanceInterpolators()
														.get(guidType),
												Float.NaN,
												forcedPfafs,
												resource.getGuidSourceExpiration());
								forced = true;
							} else if (pfafList.size() > 1) {
								guidance = guidRecords
										.get(guidType)
										.getBasinData("ALL")
										.getAverageGuidanceValue(
												pfafList,
												resource.getGuidanceInterpolators()
														.get(guidType),
												Float.NaN,
												forcedPfafs,
												resource.getGuidSourceExpiration());
								if (forcedPfafs.size() > 0) {
									forced = true;
								}
							} else {
								guidance = resource.getGuidanceValue(
										ffmpGuidBasin, monitor.getQpeWindow()
												.getBeforeTime(), guidType);

								if (guidance < 0.0f) {
									guidance = Float.NaN;
								}
							}

							trd.setTableCellData(i + 4, new FFMPTableCellData(
									FIELDS.GUIDANCE, guidance, forced));
						} else {
							// check for forcing even if no data are available
							guidance = getForcedAvg(forceUtil, domain, cBasin,
									guidType);
							if (guidance.isNaN() == false) {
								forced = true;
							} else {
								forced = false;
							}

							trd.setTableCellData(i + 4, new FFMPTableCellData(
									FIELDS.GUIDANCE, guidance, forced));
						}

						if (!qpe.isNaN() && (guidance > 0.0f)) {
							trd.setTableCellData(
									i + 5,
									new FFMPTableCellData(FIELDS.RATIO,
											FFMPUtils.getRatioValue(qpe,
													guidance)));
							trd.setTableCellData(
									i + 6,
									new FFMPTableCellData(FIELDS.DIFF,
											FFMPUtils.getDiffValue(qpe,
													guidance)));
						} else {
							trd.setTableCellData(i + 5, new FFMPTableCellData(
									FIELDS.RATIO, Float.NaN));
							trd.setTableCellData(i + 6, new FFMPTableCellData(
									FIELDS.DIFF, Float.NaN));
						}

						i += 3;
					}
				} else {
					trd = getMaxValue(trd, cBasin);
				}

				tData.addDataRow(trd);
			}
		}
	}

	private float getForcedAvg(FFFGForceUtil forceUtil, String domain,
			FFMPBasin cBasin, String guidType) {
		FFFGDataMgr fdm = FFFGDataMgr.getInstance();
		ArrayList<Long> forcedPfafs;
		ArrayList<Long> pfafList = new ArrayList<Long>();
		float guidance = Float.NaN;

		boolean forced = false;
		if (fdm.isForcingConfigured()) {
			forceUtil.calculateForcings(domain, ft, cBasin);
			forcedPfafs = forceUtil.getForcedPfafList();
			forced = forceUtil.isForced();
			if (forced == false) {
				return Float.NaN;
			}
		} else {
			return Float.NaN;
		}

		if (cBasin.getAggregated()) {
			if (domain == null) {
				pfafList = ft.getAggregatePfafs(cBasin.getPfaf(),
						resource.getSiteKey(), resource.getHuc());
			} else if (!domain.equals("NA")) {
				if (!resource.getHuc().equals("ALL")) {
					pfafList = ft.getAggregatePfafsByDomain(cBasin.getPfaf(),
							resource.getSiteKey(), domain, resource.getHuc());
				}
			} else {
				pfafList = ft.getAggregatePfafsByDomain(cBasin.getPfaf(),
						resource.getSiteKey(), domain, resource.getHuc());
				pfafList.add(ft.getAggregatedPfaf(cBasin.getPfaf(),
						resource.getSiteKey(), resource.getHuc()));
			}
		}

		if (!resource.isWorstCase() || resource.getHuc().equals("ALL")
				|| (resource.centeredAggregationKey != null)) {
			if (((forcedPfafs.size() > 1)) || forced) {
				// Calculate an average
				guidance = forceUtil.getAvgForcedValue(pfafList, forcedPfafs,
						resource.getGuidanceInterpolators().get(guidType),
						resource.getGuidSourceExpiration(), ft);
				// } else if (forcedPfafs.size() > 1) {
				// guidance = forceUtil.getAvgForcedValue(pfafList,
				// forcedPfafs,
				// resource.getGuidanceInterpolators().get(guidType),
				// resource.getGuidSourceExpiration(), ft);
				// forced = true;
			}
		} else {
			// TODO Calculate a max value

		}

		return guidance;
	}

	/**
	 * Regular basin display name
	 * 
	 * @param basin
	 * @return
	 */
	private String getDisplayName(FFMPBasin basin) {
		String name = null;

		try {
			if (resource.getHuc().equals("ALL")
					|| (resource.centeredAggregationKey != null)) {
				name = ft.getBasin(resource.getSiteKey(), basin.getPfaf())
						.getStreamName();
			}
			// aggregations
			else {

				ArrayList<Long> pfafs = ft.getAggregatePfafs(basin.getPfaf(),
						resource.getSiteKey(), resource.getHuc());
				if (pfafs.size() > 0) {
					if (resource.getHuc().equals("COUNTY")) {
						name = ft.getCountyStateName(resource.getSiteKey(),
								basin.getPfaf());
					} else {
						for (int i = 0; i < pfafs.size(); i++) {
							if (ft.getBasin(resource.getSiteKey(), pfafs.get(0))
									.getHucName() != null) {
								name = ft.getBasin(resource.getSiteKey(),
										pfafs.get(0)).getHucName();
								break;
							}
						}
					}
				}
			}
		} catch (Exception e) {
			statusHandler.handle(Priority.WARN, "No display name for basin.."
					+ basin.getPfaf());
		}
		return name;
	}

	private FFMPTableRowData getMaxValue(FFMPTableRowData trd, FFMPBasin cBasin) {
		ArrayList<DomainXML> domainList = FFMPRunConfigurationManager
				.getInstance().getDomains();
		ArrayList<DomainXML> activeDomains = new ArrayList<DomainXML>();
		for (DomainXML domainXml : domainList) {
			for (String cwa : cwaArr) {
				if (domainXml.getCwa().equalsIgnoreCase(cwa)) {
					activeDomains.add(domainXml);
					break;
				}
			}
		}

		ArrayList<Long> pfafs = ft.getAggregatePfafs(cBasin.getPfaf(),
				resource.getSiteKey(), resource.getHuc(), activeDomains);
		trd.setPfaf(cBasin.getPfaf().toString());
		Float qpe = Float.NaN;
		Float guidance = Float.NaN;
		Float rate = Float.NaN;
		Float qpf = Float.NaN;

		if (cBasin instanceof FFMPVirtualGageBasin) {
			if (pfafs.size() == 0) {
				if (virtualBasin != null) {
					trd.setTableCellData(
							1,
							new FFMPTableCellData(FIELDS.RATE, virtualBasin
									.get(cBasin.getPfaf()).getValue(
											resource.getPaintTime().getRefTime())));
				} else {
					trd.setTableCellData(1, new FFMPTableCellData(FIELDS.RATE,
							Float.NaN));
				}
				if (virtualBasin != null) {
					if (resource.getTime() > 0.00) {
						qpe = virtualBasin.get(cBasin.getPfaf()).getAccumValue(
								monitor.getQpeWindow().getAfterTime(),
								monitor.getQpeWindow().getBeforeTime(),
								expirationTime, isRate);
					} else {
						qpe = 0.0f;
					}

					trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
							qpe));

				} else {
					trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
							Float.NaN));
				}
				if (qpfBasin != null) {
					trd.setTableCellData(
							3,
							new FFMPTableCellData(FIELDS.QPF, new Float(
									qpfBasin.get(cBasin.getPfaf()).getMaxValue(
											monitor.getQpfWindow()
													.getAfterTime(),
											monitor.getQpfWindow()
													.getBeforeTime()))
									.floatValue()));
				} else {
					trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF,
							Float.NaN));
				}

				// run over each guidance type
				int i = 0;
				for (String guidType : guidBasins.keySet()) {
					FFFGForceUtil forceUtil = forceUtils.get(guidType);
					forceUtil.setSliderTime(resource.getTime());

					FFMPBasinData guidBasin = guidBasins.get(guidType);

					if (guidBasin != null) {

						FFMPGuidanceBasin basin = ((FFMPGuidanceBasin) guidBasin
								.get(cBasin.getPfaf()));
						guidance = resource.getGuidanceValue(basin, monitor
								.getQpeWindow().getBeforeTime(), guidType);

						forceUtil.calculateForcings(pfafs, ft, cBasin);

						ArrayList<Long> forcedPfafs = forceUtil
								.getForcedPfafList();
						boolean forced = forceUtil.isForced();

						if (!forced) {
							if ((forcedPfafs != null)
									&& (forcedPfafs.size() > 0)) {
								forced = true;
							}
						}

						trd.setTableCellData(i + 4, new FFMPTableCellData(
								FIELDS.GUIDANCE, guidance, forced));
					} else {
						trd.setTableCellData(i + 4, new FFMPTableCellData(
								FIELDS.GUIDANCE, Float.NaN));
					}
					if (!qpe.isNaN() && (guidance > 0.0f)) {

						trd.setTableCellData(
								i + 5,
								new FFMPTableCellData(FIELDS.RATIO, FFMPUtils
										.getRatioValue(qpe, guidance)));
						trd.setTableCellData(
								i + 6,
								new FFMPTableCellData(FIELDS.DIFF, FFMPUtils
										.getDiffValue(qpe, guidance)));
					} else {
						trd.setTableCellData(i + 5, new FFMPTableCellData(
								FIELDS.RATIO, Float.NaN));
						trd.setTableCellData(i + 6, new FFMPTableCellData(
								FIELDS.DIFF, Float.NaN));
					}

					i += 3;
				}
			}

		} else {
			if (pfafs.size() > 0) {
				if (rateBasin != null) {
					rate = rateBasin.getMaxValue(pfafs, resource.getPaintTime().getRefTime());
					trd.setTableCellData(1, new FFMPTableCellData(FIELDS.RATE,
							rate));
				} else {
					trd.setTableCellData(1, new FFMPTableCellData(FIELDS.RATE,
							Float.NaN));
				}
				if (qpeBasin != null) {
					qpe = qpeBasin.getAccumMaxValue(pfafs, monitor
							.getQpeWindow().getAfterTime(), monitor
							.getQpeWindow().getBeforeTime(), expirationTime,
							isRate);
					trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
							qpe));
				} else {
					trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
							Float.NaN));
				}
				if (qpfBasin != null) {
					qpf = qpfBasin.getAverageMaxValue(pfafs, monitor.getQpfWindow()
							.getAfterTime(), monitor.getQpfWindow()
							.getBeforeTime());

					// qpf = getQPFValue(true, new Long(0l), pfafs);/* DR13839
					// */
					trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF,
							qpf.floatValue()));
				} else {
					trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF,
							Float.NaN));
				}

				// run over each guidance type
				int i = 0;
				for (String guidType : guidBasins.keySet()) {
					FFFGForceUtil forceUtil = forceUtils.get(guidType);
					forceUtil.setSliderTime(resource.getTime());

					FFMPBasinData guidBasin = guidBasins.get(guidType);

					ArrayList<Long> pfafList = new ArrayList<Long>();
					if ((guidBasin != null)
							&& (guidBasin.getBasins().size() > 0)) {
						if (cBasin.getAggregated()) {
							pfafList = ft.getAggregatePfafs(cBasin.getPfaf(),
									resource.getSiteKey(), resource.getHuc());
							pfafList.add(ft.getAggregatedPfaf(cBasin.getPfaf(),
									resource.getSiteKey(), resource.getHuc()));
						}

						boolean forced = false;
						ArrayList<Long> forcedPfafs = new ArrayList<Long>();
						FFFGDataMgr fdm = FFFGDataMgr.getInstance();

						if (fdm.isForcingConfigured()) {
							forceUtil.calculateForcings(pfafList, ft, cBasin);
							forcedPfafs = forceUtil.getForcedPfafList();
							forced = forceUtil.isForced();
						}

						if (!forced) {
							if ((forcedPfafs != null)
									&& (forcedPfafs.size() > 0)) {
								forced = true;
							}
						}

						if (resource.isWorstCase()) {
							guidance = guidRecords
									.get(guidType)
									.getBasinData("ALL")
									.getMaxGuidanceValue(
											pfafs,
											resource.getGuidanceInterpolators()
													.get(guidType),
											resource.getGuidSourceExpiration(),
											cBasin.getPfaf());
						} else {
							FFMPGuidanceBasin basin = (FFMPGuidanceBasin) guidRecords
									.get(guidType)
									.getBasinData(resource.getHuc())
									.get(cBasin.getPfaf());
							guidance = resource.getGuidanceValue(basin, monitor
									.getQpeWindow().getBeforeTime(), guidType);
						}

						trd.setTableCellData(i + 4, new FFMPTableCellData(
								FIELDS.GUIDANCE, guidance, forced));
					} else {
						trd.setTableCellData(i + 4, new FFMPTableCellData(
								FIELDS.GUIDANCE, Float.NaN));
					}
					if (!qpe.isNaN() && (guidance > 0.0f)) {

						ArrayList<Float> qpes = qpeBasin.getAccumValues(pfafs,
								monitor.getQpeWindow().getAfterTime(), monitor
										.getQpeWindow().getBeforeTime(),
								expirationTime, isRate);
						ArrayList<Float> guids = null;
						if (guidBasin != null) {
							guids = guidBasin.getGuidanceValues(pfafs, resource
									.getGuidanceInterpolators().get(guidType),
									resource.getGuidSourceExpiration());
						}

						if ((qpes.size() > 0)
								&& ((guids != null) && (guids.size() > 0))) {

							trd.setTableCellData(
									i + 5,
									new FFMPTableCellData(FIELDS.RATIO,
											FFMPUtils.getMaxRatioValue(qpes,
													guids)));
							trd.setTableCellData(
									i + 6,
									new FFMPTableCellData(FIELDS.DIFF,
											FFMPUtils.getMaxDiffValue(qpes,
													guids)));
						} else {
							trd.setTableCellData(i + 5, new FFMPTableCellData(
									FIELDS.RATIO, Float.NaN));
							trd.setTableCellData(i + 6, new FFMPTableCellData(
									FIELDS.DIFF, Float.NaN));
						}
					} else {
						trd.setTableCellData(i + 5, new FFMPTableCellData(
								FIELDS.RATIO, Float.NaN));
						trd.setTableCellData(i + 6, new FFMPTableCellData(
								FIELDS.DIFF, Float.NaN));
					}

					i += 3;
				}

			} else {
				if ((rateBasin != null)
						&& (rateBasin.get(cBasin.getPfaf()) != null)) {
					trd.setTableCellData(
							1,
							new FFMPTableCellData(FIELDS.RATE, rateBasin.get(
									cBasin.getPfaf()).getValue(resource.getPaintTime().getRefTime())));
				} else {
					trd.setTableCellData(1, new FFMPTableCellData(FIELDS.RATE,
							Float.NaN));
				}
				if ((qpeBasin != null)
						&& (qpeBasin.get(cBasin.getPfaf()) != null)) {
					qpe = qpeBasin.get(cBasin.getPfaf()).getAccumValue(
							monitor.getQpeWindow().getAfterTime(),
							monitor.getQpeWindow().getBeforeTime(),
							expirationTime, isRate);
					trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
							qpe));
				} else {
					trd.setTableCellData(2, new FFMPTableCellData(FIELDS.QPE,
							Float.NaN));
				}
				if ((qpfBasin != null)
						&& (qpfBasin.get(cBasin.getPfaf()) != null)) {
					trd.setTableCellData(
							3,
							new FFMPTableCellData(FIELDS.QPF, new Float(
									qpfBasin.get(cBasin.getPfaf()).getMaxValue(
											monitor.getQpfWindow()
													.getAfterTime(),
											monitor.getQpfWindow()
													.getBeforeTime()))
									.floatValue()));
				} else {
					trd.setTableCellData(3, new FFMPTableCellData(FIELDS.QPF,
							Float.NaN));
				}

				// run over each guidance type
				int i = 0;
				for (String guidType : guidBasins.keySet()) {
					FFFGForceUtil forceUtil = forceUtils.get(guidType);
					forceUtil.setSliderTime(resource.getTime());

					FFMPBasinData guidBasin = guidBasins.get(guidType);

					if (guidBasin != null) {

						FFMPGuidanceBasin basin = ((FFMPGuidanceBasin) guidBasin
								.get(cBasin.getPfaf()));
						guidance = resource.getGuidanceValue(basin, monitor
								.getQpeWindow().getBeforeTime(), guidType);

						if (guidance < 0.0f) {
							guidance = Float.NaN;
						}

						forceUtil.calculateForcings(pfafs, ft, cBasin);

						ArrayList<Long> forcedPfafs = forceUtil
								.getForcedPfafList();
						boolean forced = forceUtil.isForced();

						if (!forced) {
							if ((forcedPfafs != null)
									&& (forcedPfafs.size() > 0)) {
								forced = true;
							}
						}

						trd.setTableCellData(i + 4, new FFMPTableCellData(
								FIELDS.GUIDANCE, guidance, forced));
					} else {
						trd.setTableCellData(i + 4, new FFMPTableCellData(
								FIELDS.GUIDANCE, Float.NaN));
					}
					if (!qpe.isNaN() && (guidance > 0.0f)) {

						trd.setTableCellData(
								i + 5,
								new FFMPTableCellData(FIELDS.RATIO, FFMPUtils
										.getRatioValue(qpe, guidance)));
						trd.setTableCellData(
								i + 6,
								new FFMPTableCellData(FIELDS.DIFF, FFMPUtils
										.getDiffValue(qpe, guidance)));
					} else {
						trd.setTableCellData(i + 5, new FFMPTableCellData(
								FIELDS.RATIO, Float.NaN));
						trd.setTableCellData(i + 6, new FFMPTableCellData(
								FIELDS.DIFF, Float.NaN));
					}

					i += 3;
				}
			}

		}

		return trd;
	}

	/**
	 * Gets the base field
	 * 
	 * @return
	 * @throws VizException
	 */
	private FIELDS getBaseField() {
		FIELDS field = null;
		String huc = null;
		dman = FFFGDataMgr.getInstance();

		FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
				.getTableConfigData(resource.getSiteKey());
		String qpfType = ffmpTableCfgData.getQpfType();
		ProductRunXML productRun = FFMPRunConfigurationManager.getInstance()
				.getProduct(resource.getSiteKey());
		String qpfSource = productRun
				.getQpfSources(resource.getProduct(), qpfType).get(0)
				.getSourceName();

		FFMPConfig config = FFMPConfig.getInstance();
		String includedCWAs = config.getFFMPConfigData().getIncludedCWAs();
		cwaArr = includedCWAs.split(",");
		monitor.setQpfWindow(monitor.getTimeWindow(qpfSource, resource
				.getPaintTime().getRefTime(), resource.getSiteKey()));
		Date qpeTime = resource.getPaintTime().getRefTime();
		if (resource.isSplit()) {
			// hack off the QPF duration for the table values of QPE (Split
			// Window)
			double duration = FFMPSourceConfigurationManager.getInstance()
					.getSource(qpfSource).getDurationHour();
			qpeTime = new Date((long) (resource.getPaintTime().getRefTime()
					.getTime() - (duration * 3600 * 1000)));
		}

		monitor.setQpeWindow(new FFMPTimeWindow(resource.getTableTime(),
				qpeTime));
		// set keys
		String siteKey = resource.getSiteKey();
		String dataKey = resource.getDataKey();
		ProductXML product = resource.getProduct();

		if (resource.isWorstCase() || (resource.centeredAggregationKey != null)) {
			// make sure that "ALL" is loaded
			huc = "ALL";

			rateRecord = monitor.getRateRecord(product, siteKey, dataKey,
					product.getRate(), resource.getPaintTime().getRefTime(),
					huc, true);
			qpeRecord = monitor.getQPERecord(product, siteKey, dataKey,
					product.getQpe(), resource.getTableTime(), huc, true);
			qpfRecord = monitor.getQPFRecord(product, siteKey, dataKey, null,
					resource.getPaintTime().getRefTime(), huc, true);
			guidRecords = monitor.getGuidanceRecords(product, siteKey,
					resource.getTableTime(), huc, true);
			virtualRecord = monitor.getVirtualRecord(product, siteKey, dataKey,
					product.getVirtual(), resource.getTableTime(), huc, true);
		} else {
			rateRecord = monitor.getRateRecord(product, siteKey, dataKey,
					product.getRate(), resource.getPaintTime().getRefTime(),
					resource.getHuc(), true);
			qpeRecord = monitor.getQPERecord(product, siteKey, dataKey,
					product.getQpe(), resource.getTableTime(),
					resource.getHuc(), true);
			qpfRecord = monitor.getQPFRecord(product, siteKey, dataKey, null,
					resource.getPaintTime().getRefTime(), resource.getHuc(), true);
			guidRecords = monitor.getGuidanceRecords(product, siteKey,
					resource.getTableTime(), resource.getHuc(), true);
			if (resource.getHuc().equals("ALL")) {
				virtualRecord = monitor.getVirtualRecord(product, siteKey,
						dataKey, product.getVirtual(), resource.getTableTime(),
						resource.getHuc(), true);
			}
			huc = resource.getHuc();
		}

		try {
			if (rateRecord != null) {
				rateBasin = rateRecord.getBasinData(huc);
				if (rateBasin.getBasins().size() > 0) {
					field = FIELDS.RATE;
					baseRec = rateRecord;
				}
			}
			if (qpeRecord != null) {
				qpeBasin = qpeRecord.getBasinData(huc);
				if (qpeBasin.getBasins().size() > 0) {
					field = FIELDS.QPE;
					if (baseRec == null) {
						baseRec = qpeRecord;
					}
				}
			}
			if (qpfRecord != null) {
				qpfBasin = qpfRecord.getBasinData(huc);
			}
			if (guidRecords != null) {
				guidBasins = new HashMap<String, FFMPBasinData>();
				for (String type : guidRecords.keySet()) {
					if (guidRecords.get(type) != null) {
						guidBasins.put(type, guidRecords.get(type)
								.getBasinData(huc));
					} else {
						guidBasins.put(type, null);
					}
				}
			}
			if (virtualRecord != null) {
				virtualBasin = virtualRecord.getBasinData(huc);
			}

			// Get interpolators
			HashMap<String, FFMPGuidanceInterpolation> interpolators = resource
					.getGuidanceInterpolators();
			if ((forceUtils == null) || (forceUtils.size() == 0)) {
				forceUtils = new HashMap<String, FFFGForceUtil>();

				for (String guidType : interpolators.keySet()) {
					FFFGForceUtil fu = new FFFGForceUtil(resource, guidType);
					forceUtils.put(guidType, fu);
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
			statusHandler.handle(Priority.WARN, "field Not Available");
		}

		return field;
	}

}
