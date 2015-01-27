# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# TCV_Dictionary
#   TCV_Dictionary file
# Author: GFE Installation Script
# ----------------------------------------------------------------------------
#  Needed to prevent an error from the SmartTool module
WeatherElementEdited = None

ThreatStatements = {
    "Wind": {
        "Extreme": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for major hurricane force wind greater than 110 MPH of equivalent Category 3, 4, or 5 intensity.",
                "preparation": "To be safe, aggressively prepare for the potential of devastating to catastrophic wind impacts. Efforts should now be underway to secure all properties.",
                "action": "Life threatening wind is possible. Failure to adequately shelter may result in serious injury, loss of life, or immense human suffering.",
            },
            "complete preparations": {
                "planning": "Adjustments to emergency plans should include a reasonable threat for major hurricane force wind greater than 110 MPH of equivalent Category 3, 4, or 5 intensity.",
                "preparation": "To be safe, aggressively prepare for the potential of devastating to catastrophic wind impacts. Remaining efforts to secure properties should now be brought to completion.",
                "action": "Life threatening wind is possible. Failure to adequately shelter may result in serious injury, loss of life, or immense human suffering. Move to safe shelter before the wind becomes hazardous.",
            },
            "hunker down": {
                "planning": "Brace against the reasonable threat for major hurricane force wind greater than 110 MPH of equivalent Category 3, 4, or 5 intensity. Maintain readiness for emergency response.",
                "preparation": "To be safe, last minute efforts should fully focus on protecting life. Efforts to secure properties against devastating to catastrophic wind impacts should now be complete.",
                "action": "Life threatening wind is imminent or ongoing. Now is the time to urgently hide from the wind. Failure to adequately shelter may result in serious injury, loss of life, or immense human suffering. Remain sheltered until the hazardous wind subsides. Be ready to quickly move to the safest place within your shelter if extreme wind warnings are issued.",
            },
            "recovery": {
                "planning": "The threat of hazardous wind has subsided.",
                "preparation": "To be safe, heed the instructions of local officials when moving about. Stay out of restricted areas.",
                "action": "Failure to exercise due safety may result in additional injuries or loss of life.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "High": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for hurricane force wind of 74 to 110 MPH of equivalent Category 1 or 2 intensity.",
                "preparation": "To be safe, aggressively prepare for the potential of extensive wind impacts. Efforts should now be underway to secure all properties.",
                "action": "Life threatening wind is possible. Failure to adequately shelter may result in serious injury or loss of life.",
            },
            "complete preparations": {
                "planning": "Adjustments to emergency plans should include a reasonable threat for hurricane force wind of 74 to 110 MPH of equivalent Category 1 or 2 intensity.",
                "preparation": "To be safe, aggressively prepare for the potential of extensive wind impacts. Remaining efforts to secure properties should now be brought to completion.",
                "action": "Life threatening wind is possible. Failure to adequately shelter may result in serious injury or loss of life. Move to safe shelter before the wind becomes hazardous.",
            },
            "hunker down": {
                "planning": "Brace against the reasonable threat for hurricane force wind of 74 to 110 MPH of equivalent Category 1 or 2 intensity.",
                "preparation": "To be safe, last minute efforts should fully focus on protecting life. Efforts to secure properties against extensive wind impacts should now be complete.",
                "action": "Life threatening wind is imminent or ongoing. Now is the time to urgently hide from the wind. Failure to adequately shelter may result in serious injury or loss of life. Remain sheltered until the hazardous wind subsides.",
            },
            "recovery": {
                "planning": "The threat for hazardous wind has subsided.",
                "preparation": "To be safe, heed the instructions of local officials when moving about. Stay out of restricted areas.",
                "action": "Failure to exercise due safety may result in additional injuries or loss of life.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "Mod": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for strong tropical storm force wind of 58 to 73 MPH.",
                "preparation": "To be safe, earnestly prepare for the potential of significant wind impacts. Efforts should now be underway to secure all properties.",
                "action": "Failure to adequately shelter may result in serious injury, or in some cases loss of life.",
            },
            "complete preparations": {
                "planning": "Adjustments to emergency plans should include a reasonable threat for strong tropical storm force wind of 58 to 73 MPH.",
                "preparation": "To be safe, earnestly prepare for the potential of significant wind impacts. Remaining efforts to secure properties should now be brought to completion.",
                "action": "Dangerous wind is possible. Failure to adequately shelter may result in serious injury, or in some cases loss of life. Move to safe shelter before the wind becomes hazardous.",
            },
            "hunker down": {
                "planning": "Brace against the reasonable threat for strong tropical storm force wind of 58 to 73 MPH.",
                "preparation": "To be safe, last minute efforts should fully focus on protecting life. Efforts to secure properties against significant wind impacts should now be complete.",
                "action": "Dangerous wind is imminent or ongoing. Now is the time to hide from the wind. Failure to adequately shelter may result in serious injury, or in some cases loss of life. Remain sheltered until the hazardous wind subsides.",
            },
            "recovery": {
                "planning": "The threat for hazardous wind has subsided.",
                "preparation": "To be safe, heed the instructions of local officials when moving about. Stay out of restricted areas.",
                "action": "Failure to exercise due safety may result in additional injuries, or in some cases loss of life.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "Elevated": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for tropical storm force wind of 39 to 57 MPH.",
                "preparation": "To be safe, prepare for the potential of limited wind impacts. Efforts should now be underway to secure all properties.",
                "action": "Hazardous wind is possible. Failure to adequately shelter may result in serious injury.",
            },
            "complete preparations": {
                "planning": "Adjustments to emergency plans should include a reasonable threat for tropical storm force wind of 39 to 57 MPH.",
                "preparation": "To be safe, prepare for the potential of limited wind impacts. Remaining efforts to secure properties should now be brought to completion.",
                "action": "Hazardous wind is possible. Failure to adequately shelter may result in serious injury. Move to safe shelter before the wind becomes hazardous.",
            },
            "hunker down": {
                "planning": "Brace against the reasonable threat for tropical storm force wind of 39 to 57 MPH. Maintain a readiness for emergency response.",
                "preparation": "To be safe, last minute efforts should fully focus on avoiding injury. Efforts to secure properties against limited wind impacts should now be complete.",
                "action": "Hazardous wind is imminent or ongoing. Now is the time to hide from the wind. Failure to adequately shelter may result in serious injury. Remain sheltered until the hazardous wind subsides.",
            },
            "recovery": {
                "planning": "The threat for hazardous wind has subsided.",
                "preparation": "To be safe, heed the instructions of local officials when moving about. Stay out of restricted areas.",
                "action": "Failure to exercise due safety may result in additional injuries.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "None": {
            "check plans": {
                "planning": "Emergency planning for this event need not include a threat for tropical storm force wind. The wind will remain less than 39 MPH, but conditions may still be breezy to windy.",
                "preparation": "Little to no preparations needed to guard against tropical wind.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical wind event.",
            },
            "complete preparations": {
                "planning": "Emergency planning for this event need not include a threat for tropical storm force wind. The wind will remain less than 39 MPH, but conditions may still be breezy to windy.",
                "preparation": "Little to no preparations needed to guard against tropical wind.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical wind event.",
            },
            "hunker down": {
                "planning": "The wind will remain less than 39 MPH, but conditions may still be breezy to windy.",
                "preparation": "Little to no preparations needed to guard against tropical wind.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical wind event.",
            },
            "recovery": {
                "planning": "Conditions may be still breezy to windy.",
                "preparation": "Exercise due safety when moving about.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical wind event.",
            },
            "nothing to see here": {
                "planning": "Conditions may be breezy to windy.",
                "preparation": "Exercise due safety when moving about.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical wind event.",
            },
        },
    },
    "Storm Surge": {
        "Extreme": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for extreme storm surge flooding greater than 9 feet above ground.",
                "preparation": "To be safe, aggressively prepare for the potential of devastating to catastrophic storm surge flooding impacts. Evacuation efforts should now be underway.",
                "action": "Life threatening inundation is possible. Failure to heed evacuation orders may result in serious injury, significant loss of life, or immense human suffering. Leave if evacuation orders are given for your area. Consider voluntary evacuation if recommended. Poor decisions may result in being cut off or needlessly risk lives.",
            },
            "complete preparations": {
                "planning": "Adjustments to emergency plans should include a reasonable threat for extreme storm surge flooding greater than 9 feet above ground.",
                "preparation": "To be safe, aggressively prepare for the potential of devastating to catastrophic storm surge flooding impacts. Evacuation efforts should now be brought to completion. Evacuations must be complete before driving conditions become unsafe.",
                "action": "Life threatening inundation is possible. Failure to heed evacuation orders may result in serious injury, significant loss of life, or immense human suffering. Leave immediately if evacuation orders have been given for your area. Consider voluntary evacuation if recommended. Poor decisions may result in being cut off or needlessly risk lives.",
            },
            "hunker down": {
                "planning": "Emergency response should posture for a reasonable threat for extreme storm surge flooding greater than 9 feet above ground.",
                "preparation": "To be safe, evacuees should now be located within prescribed shelters and well away from deadly storm surge flooding capable of devastating to catastrophic impacts.",
                "action": "Life threatening inundation is imminent or ongoing. Failure to have heeded evacuation orders may result in serious injury, significant loss of life, or immense human suffering.",
            },
            "recovery": {
                "planning": "The threat of deadly storm surge is abating as flood waters recede.",
                "preparation": "To be safe, heed the instructions of local officials when moving about. Do not return to evacuated areas until flood waters completely recede and the all-clear is officially given.",
                "action": "Failure to exercise due safety may result in additional injuries or loss of life.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "High": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for major storm surge flooding of 6 to 9 feet above ground.",
                "preparation": "To be safe, aggressively prepare for the potential of extensive storm surge flooding impacts. Evacuate efforts should now be underway.",
                "action": "Life threatening inundation is possible. Failure to heed evacuation orders may result in serious injury, significant loss of life, or human suffering. Leave if evacuation orders are given for your area. Consider voluntary evacuation if recommended. Poor decisions may result in being cut off or needlessly risk lives.",
            },
            "complete preparations": {
                "planning": "Adjustments to emergency plans should include a reasonable threat for major storm surge flooding of 6 to 9 feet above ground.",
                "preparation": "To be safe, aggressively prepare for the potential of extensive storm surge flooding impacts. Evacuation efforts should now be brought to completion. Evacuations must be complete before driving conditions become unsafe.",
                "action": "Life threatening inundation is possible. Failure to heed evacuation orders may result in serious injury, significant loss of life, or human suffering. Leave if evacuation orders are given for your area. Consider voluntary evacuation if recommended. Poor decisions may result in being cut off or needlessly risk lives.",
            },
            "hunker down": {
                "planning": "Emergency response should posture for a reasonable threat for major storm surge flooding of 6 to 9 feet above ground.",
                "preparation": "To be safe, evacuees should now be located within prescribed shelters and well away from deadly storm surge flooding capable of extensive impacts.",
                "action": "Life threatening inundation is imminent or ongoing. Failure to have heeded evacuation orders may result in serious injury, significant loss of life, or human suffering.",
            },
            "recovery": {
                "planning": "The threat of deadly storm surge is abating as flood waters recede.",
                "preparation": "To be safe, heed the instructions of local officials when moving about. Do not return to evacuated areas until flood waters completely recede and the all-clear is officially given.",
                "action": "Failure to exercise due safety may result in additional injuries or loss of life.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "Mod": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for dangerous storm surge flooding of 3 to 6 feet above ground.",
                "preparation": "To be safe, earnestly prepare for the potential of significant storm surge flooding impacts. Evacuation efforts should now be underway.",
                "action": "Life threatening inundation is possible. Failure to heed evacuation orders or instructions from local officials may result in serious injury or loss of life. Leave if evacuation orders are given for your area. Consider voluntary evacuation if recommended. Poor decisions may needlessly risk lives.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for dangerous storm surge flooding of 3 to 6 feet above ground.",
                "preparation": "To be safe, earnestly prepare for the potential of significant storm surge flooding impacts. Evacuation efforts should now be brought to completion. Evacuations must be complete before driving conditions become unsafe.",
                "action": "Life threatening inundation is possible. Failure to heed evacuation orders or instructions from local officials may result in serious injury or loss of life. Leave if evacuation orders are given for your area. Consider voluntary evacuation if recommended. Poor decisions may needlessly risk lives.",
            },
            "hunker down": {
                "planning": "Emergency response should posture for a reasonable threat for dangerous storm surge flooding of 3 to 6 feet above ground.",
                "preparation": "To be safe, evacuees should now be located within prescribed shelters and well away from storm surge flooding capable of significant impacts.",
                "action": "Life threatening inundation is imminent or ongoing. Failure to have heeded evacuation orders or instructions from local officials may result in serious injury or loss of life.",
            },
            "recovery": {
                "planning": "The threat of dangerous storm surge is abating as flood waters recede.",
                "preparation": "To be safe, heed the instructions of local officials when moving about. Do not return to evacuated areas until flood waters completely recede and the all-clear is officially given.",
                "action": "Failure to exercise due safety may result in additional injuries or loss of life.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "Elevated": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for peak storm surge flooding of 1 to 3 feet above ground.",
                "preparation": "To be safe, prepare for the potential of limited storm surge flooding impacts. Preparedness efforts should be underway.",
                "action": "Localized inundation is possible. Follow the instructions of local officials. Consider voluntary evacuation if recommended. Leave if evacuation orders are issued.",
            },
            "complete preparations": {
                "planning": "Adjustments to emergency plans should include a reasonable threat for peak storm surge flooding of 1 to 3 feet above ground.",
                "preparation": "To be safe, prepare for the potential of limited storm surge flooding impacts. Preparedness efforts should now be brought to completion before conditions deteriorate.",
                "action": "Localized inundation is possible. Follow the instructions of local officials. Consider voluntary evacuation if recommended. Leave immediately if evacuation orders are issued.",
            },
            "hunker down": {
                "planning": "Emergency response should posture for a reasonable threat for peak storm surge flooding of 1 to 3 feet above ground.",
                "preparation": "To be safe, stay away from storm surge flooding capable of limited impacts.",
                "action": "Localized inundation is imminent or ongoing. Continue to follow the instructions of local officials.",
            },
            "recovery": {
                "planning": "The threat of hazardous storm surge is abating as flood waters recede.",
                "preparation": "To be safe, heed the instructions of local officials when moving about. Do not return to flooded areas until the all-clear is officially given.",
                "action": "Exercise due safety.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "None": {
            "check plans": {
                "planning": "Emergency planning for this event need not include a threat for storm surge flooding. The ground will remain largely unflooded from surge water or only have spots minimally affected by surge encroachment. Surf conditions may still be rough with some beach erosion. Stronger than normal rip currents may also be present.",
                "preparation": "Little to no preparations needed to guard against storm surge flooding.",
                "action": "Review your seasonal plan and ensure readiness for the next storm surge event.",
            },
            "complete preparations": {
                "planning": "Emergency planning for this event need not include a threat for storm surge flooding. The ground will remain largely unflooded from surge water or only have spots minimally affected by surge encroachment. Surf conditions may still be rough with some beach erosion. Stronger than normal rip currents may also be present.",
                "preparation": "Little to no preparations needed to guard against storm surge flooding.",
                "action": "Review your seasonal plan and ensure readiness for the next storm surge event.",
            },
            "hunker down": {
                "planning": "The ground will remain largely unflooded from surge water or only have spots minimally affected by surge encroachment. Surg conditions may still be rough with some beach erosion. Stronger than normal rip currents may also be present.",
                "preparation": "Little to no preparations needed to guard against storm surge flooding.",
                "action": "Review your seasonal plan and ensure readiness for the next storm surge event.",
            },
            "recovery": {
                "planning": "Surf conditions may be rough with some beach erosion. Stronger than normal rip currents may also be present.",
                "preparation": "Exercise due safety.",
                "action": "Review your seasonal plan and ensure readiness for the next storm surge event.",
            },
            "nothing to see here": {
                "planning": "Surf conditions may be rough with some beach erosion. Stronger than normal rip currents may also be present. ",
                "preparation": "Exercise due safety.",
                "action": "Review your seasonal plan and ensure readiness for the next storm surge event.",
            },
        },
    },
    "Flooding Rain": {
        "Extreme": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat of extreme flooding where peak rainfall totals vastly exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are very likely. ",
                "preparation": "To be safe, aggressively prepare for the potential of devastating to catastrophic flooding rain impacts.",
                "action": "Life threatening flooding is possible. Failure to take action may result in serious injury, significant loss of life, or human suffering. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers. Poor decisions may result in being cut off or needlessly risk lives. If vulnerable, relocate to safe shelter on higher ground.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for extreme flooding where peak rainfall totals vastly exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are very likely.",
                "preparation": "To be safe, aggressively prepare for the potential of devastating to catastrophic flooding rain impacts.",
                "action": "Life threatening flooding is possible. Failure to take action may result in serious injury, significant loss of life, and human suffering. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers. Poor decisions may result in being cut off or needlessly risk lives. If vulnerable, relocate to safe shelter on higher ground.",
            },
            "hunker down": {
                "planning": "Emergency plans should include a reasonable threat for extreme flooding where peak rainfall totals vastly exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are very likely.",
                "preparation": "To be safe, remain prepared for the potential of devastating to catastrophic flooding rain impacts.",
                "action": "Life threatening flooding is possible. Failure to take action may result in serious injury, significant loss of life, and human suffering. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers.",
            },
            "recovery": {
                "planning": "Emergency plans should include a reasonable threat for extreme flooding where peak rainfall totals vastly exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are very likely.",
                "preparation": "To be safe, remain prepared for the potential of devastating to catastrophic flooding rain impacts.",
                "action": "Life threatening flooding is possible. Failure to take action may result in serious injury, significant loss of life, and human suffering. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "High": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for major flooding where peak rainfall totals well exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are likely.",
                "preparation": "To be safe, aggressively prepare for the potential of extensive flooding rain impacts.",
                "action": "Life threatening flooding is possible. Failure to take action may result in serious injury or significant loss of life. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers. Poor decisions may result in being cut off or needlessly risk lives. If vulnerable, relocate to safe shelter on higher ground.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for major flooding where peak rainfall totals well exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are likely.",
                "preparation": "To be safe, aggressively prepare for the potential of extensive flooding rain impacts. Life threatening flooding possible from excessive tropical rain.",
                "action": "Life threatening flooding is possible. Failure to take action may result in serious injury or significant loss of life. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers. Poor decisions may result in being cut off or needlessly risk lives. If vulnerable, relocate to safe shelter on higher ground.",
            },
            "hunker down": {
                "planning": "Emergency plans should include a reasonable threat for major flooding where peak rainfall totals well exceed amounts conducive for flash flooding and rapid inundation.",
                "preparation": "To be safe, remain prepared for the potential of extensive flooding rain impacts.",
                "action": "Life threatening flooding is possible. Failure to take action may result in serious injury or significant loss of life. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers.",
            },
            "recovery": {
                "planning": "Emergency plans should continue to include a reasonable threat for major flooding where peak rainfall totals well exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are likely.",
                "preparation": "To be safe, remain prepared for the potential of extensive flooding rain impacts.",
                "action": "Life threatening flooding is possible. Failure to take action may result in serious injury or significant loss of life. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "Mod": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for moderate flooding where peak rainfall totals notably exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are possible.",
                "preparation": "To be safe, earnestly prepare for the potential of significant flooding rain impacts.",
                "action": "Dangerous flooding is possible. Failure to take action may result in serious injury or loss of life. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for moderate flooding where peak rainfall totals notably exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are possible.",
                "preparation": "To be safe, earnestly prepare for the potential of significant flooding rain impacts.",
                "action": "Dangerous flooding is possible. Failure to take action may result in serious injury or loss of life. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers.",
            },
            "hunker down": {
                "planning": "Emergency plans should include a reasonable threat for moderate flooding where peak rainfall totals notably exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are possible.",
                "preparation": "To be safe, remain prepared for the potential of significant flooding rain impacts.",
                "action": "Dangerous flooding is possible. Failure to take action may result in serious injury or loss of life. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers.",
            },
            "recovery": {
                "planning": "Emergency plans should include a reasonable threat for moderate flooding where peak rainfall totals notably exceed amounts conducive for flash flooding and rapid inundation. Rescues and emergency evacuations are possible.",
                "preparation": "To be safe, remain prepared for the potential of significant flooding rain impacts.",
                "action": "Dangerous flooding is possible. Failure to take action may result in serious injury or loss of life. If flash flood watches and warnings are issued, heed recommended actions. Also listen for possible river flood warnings for longer-term impacts along rivers.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "Elevated": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for minor flooding where peak rainfall totals are near amounts conducive for flash flooding and rapid inundation.",
                "preparation": "To be safe, prepare for the potential of limited flooding rain impacts.",
                "action": "Localized flooding is possible. If flash flood watches and warnings are issued, heed recommended actions.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for minor flooding where peak rainfall totals are near amounts conducive for flash flooding and rapid inundation.",
                "preparation": "To be safe, prepare for the potential of limited flooding rain impacts.",
                "action": "Localized flooding is possible. If flash flood watches and warnings are issued, heed recommended actions.",
            },
            "hunker down": {
                "planning": "Emergency plans should include a reasonable threat for minor flooding where peak rainfall totals are near amounts conducive for flash flooding and rapid inundation.",
                "preparation": "To be safe, remain prepared for the potential of limited flooding rain impacts.",
                "action": "Localized flooding is possible. If flash flood watches and warnings are issued, heed recommended actions.",
            },
            "recovery": {
                "planning": "Emergency plans should include a reasonable threat for minor flooding where peak rainfall totals are near amounts conducive for flash flooding and rapid inundation.",
                "preparation": "To be safe, remain prepared for the potential of limited flooding rain impacts.",
                "action": "Localized flooding is possible. If flash flood watches and warnings are issued, heed recommended actions.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "None": {
            "check plans": {
                "planning": "Emergency planning for this event need not include a threat for rainfall flooding. Heavy rain and nuisance flooding may still occur.",
                "preparation": "Little to no preparations needed to guard against excessive tropical rainfall.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical rainfall flooding event.",
            },
            "complete preparations": {
                "planning": "Emergency planning for this event need not include a threat for rainfall flooding. Heavy rain and nuisance flooding may still occur.",
                "preparation": "Little to no preparations needed to guard against excessive tropical rainfall.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical rainfall flooding event.",
            },
            "hunker down": {
                "planning": "Emergency planning for this event need not include a threat for rainfall flooding. Heavy rain and nuisance flooding may still occur.",
                "preparation": "Little to no preparations needed to guard against excessive tropical rainfall.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical rainfall flooding event.",
            },
            "recovery": {
                "planning": "Heavy rain and nuisance flooding may still occur. ",
                "preparation": "Exercise due safety.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical rainfall flooding event.",
            },
            "nothing to see here": {
                "planning": "Heavy rain and nuisance flooding may still occur.",
                "preparation": "Exercise due safety.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical rainfall flooding event.",
            },
        },
    },
    "Tornado": {
        "Extreme": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for an outbreak of tornadoes, with several possibly strong or violent in intensity and with longer and wider damage paths. Numerous tornadoes may occur within short periods of time and in close proximity of one another.",
                "preparation": "To be safe, aggressively prepare for the potential of devastating to catastrophic tornado impacts. Those living in mobile homes should relocate to more substantial shelter. Listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or significant loss of life. Keep a watchful eye to the sky and a listening ear for warning alerts. Be ready to find shelter quickly.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for an outbreak of tornadoes, with several possibly strong or violent in intensity and with longer and wider damage paths. Numerous tornadoes may occur within short periods of time and in close proximity of one another.",
                "preparation": "To be safe, aggressively prepare for the potential of devastating to catastrophic tornado impacts. Those living in mobile homes should relocate to more substantial shelter. Listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or significant loss of life. Keep a watchful eye to the sky and a listening ear for warning alerts. Be ready to find shelter quickly.",
            },
            "hunker down": {
                "planning": "Emergency plans should include a reasonable threat for an outbreak of tornadoes, with several possibly strong or violent in intensity and with longer and wider damage paths. Numerous tornadoes may occur within short periods of time and in close proximity of one another.",
                "preparation": "To be safe, remain prepared for the potential of devastating to catastrophic tornado impacts. Stay informed and listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or significant loss of life. If tornado warnings are issued for your area, quickly move to the safest place within your shelter. Seconds can save lives.",
            },
            "recovery": {
                "planning": "Emergency plans should continue to include a reasonable threat for an outbreak of tornadoes, with several possibly strong or violent in intensity and with longer and wider damage paths. Numerous tornadoes may occur within short periods of time and in close proximity of one another.",
                "preparation": "To be safe, remain prepared for the potential of devastating to catastrophic tornado impacts. Stay informed and do not let down your guard/",
                "action": "Failure to adequately shelter may result in serious injury or significant loss of life. If tornado watches and warnings are issued, heed recommended actions.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "High": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for numerous tornadoes, with several possibly strong or violent in intensity and with longer and wider damage paths.",
                "preparation": "To be safe, aggressively prepare for the potential of extensive tornado impacts. Those living in mobile homes should relocate to more substantial shelter. Listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or significant loss of life. Keep a watchful eye to the sky and a listening ear for warning alerts. Be ready to find shelter quickly.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for numerous tornadoes, with several possibly strong or violent in intensity and with longer and wider damage paths.",
                "preparation": "To be safe, aggressively prepare for the potential of extensive tornado impacts. Those living in mobile homes should relocate to more substantial shelter. Listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or significant loss of life. Keep a watchful eye to the sky and a listening ear for warning alerts. Be ready to find shelter quickly.",
            },
            "hunker down": {
                "planning": "Emergency plans should include a reasonable threat for numerous tornadoes, with several possibly strong or violent in intensity and with longer and wider damage paths.",
                "preparation": "To be safe, remain prepared for the potential of extensive tornado impacts. Stay informed and listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or significant loss of life. If tornado warnings are issued for your area, quickly move to the safest place within your shelter. Seconds can save lives.",
            },
            "recovery": {
                "planning": "Emergency plans should include a reasonable threat for numerous tornadoes, with several possibly strong or violent in intensity and with longer and wider damage paths.",
                "preparation": "To be safe, remain prepared for the potential of extensive tornado impacts. Stay informed and do not let down your guard.",
                "action": "Failure to adequately shelter may result in serious injury or significant loss of life. If tornado watches and warnings are issued, heed recommended actions.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "Mod": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for scattered tornadoes, with a few possibly strong in intensity.",
                "preparation": "To be safe, earnestly prepare for the potential of significant tornado impacts. Listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or loss of life. Keep a watchful eye to the sky and a listening ear for warning alerts. Be ready to find shelter quickly.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for scattered tornadoes, with a few possibly strong in intensity.",
                "preparation": "To be safe, earnestly prepare for the potential of significant tornado impacts. Listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or loss of life. Keep a watchful eye to the sky and a listening ear for warning alerts. Be ready to find shelter quickly.",
            },
            "hunker down": {
                "planning": "Emergency planning should continue to include a reasonable threat for scattered tornadoes, with a few possibly strong in intensity.",
                "preparation": "To be safe, remain prepared for the potential of significant tornado impacts. Stay informed and listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury or loss of life. If tornado warnings are issued for your area, quickly move to the safest place within your shelter. Seconds can save lives.",
            },
            "recovery": {
                "planning": "Emergency planning should include a reasonable threat for scattered tornadoes, with a few possibly strong in intensity.",
                "preparation": "To be safe, remain prepared prepare for the potential of significant tornado impacts. Stay informed and do not let down your guard.",
                "action": "Failure to adequately shelter may result in serious injury or loss of life. If tornado watches and warnings are issued for your area, heed recommended actions.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "Elevated": {
            "check plans": {
                "planning": "Emergency planning should include a reasonable threat for isolated tornadoes, mostly with shorter and narrower damage paths.",
                "preparation": "To be safe, prepare for the potential of limited tornado impacts. Listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury, and in some cases loss of life. Keep a watchful eye to the sky and a listening ear for warning alerts. Be ready to find shelter quickly.",
            },
            "complete preparations": {
                "planning": "Emergency planning should include a reasonable threat for isolated tornadoes, mostly with shorter and narrower damage paths.",
                "preparation": "To be safe, prepare for the potential of limited tornado impacts. Listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury, and in some cases loss of life. Keep a watchful eye to the sky and a listening ear for warning alerts. Be ready to find shelter quickly.",
            },
            "hunker down": {
                "planning": "Emergency planning should include a reasonable threat for isolated tornadoes, mostly with shorter and narrower damage paths.",
                "preparation": "To be safe, remain prepared for the potential of limited tornado impacts. Stay informed and listen for tornado watches and warnings.",
                "action": "Failure to adequately shelter may result in serious injury, and in some cases loss of life. If tornado warnings are issued for your area, quickly move to the safest place within your shelter. Seconds can save lives.",
            },
            "recovery": {
                "planning": "Emergency planning should continue to include a reasonable threat for isolated tornadoes, mostly with shorter and narrower damage paths.",
                "preparation": "To be safe, remain prepared for the potential of limited tornado impacts. Stay informed and do not let down your guard.",
                "action": "Failure to adequately shelter may result in serious injury, and in some cases loss of life. If tornado watches and warnings are issued for your area, heed recommended actions.",
            },
            "nothing to see here": {
                "planning": "",
                "preparation": "",
                "action": "",
            },
        },
        "None": {
            "check plans": {
                "planning": "Emergency planning for this event need not include a threat for tornadoes. Showers and thunderstorms with strong wind gusts may still occur.",
                "preparation": "Little to no preparations needed to guard against tropical tornadoes.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical tornado event.",
            },
            "complete preparations": {
                "planning": "Emergency planning for this event need not include a threat for tornadoes. Showers and thunderstorms with strong wind gusts may still occur.",
                "preparation": "Little to no preparations needed to guard against tropical tornadoes.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical tornado event.",
            },
            "hunker down": {
                "planning": "Emergency plans for this event need not include a threat for tornadoes. Showers and thunderstorms with strong wind gusts may still occur.",
                "preparation": "Little to no preparations needed to guard against tropical tornadoes.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical tornado event.",
            },
            "recovery": {
                "planning": "Showers and thunderstorms with strong wind gusts may still occur.",
                "preparation": "Exercise due safety when moving about.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical tornado event.",
            },
            "nothing to see here": {
                "planning": "Showers and thunderstorms with strong wind gusts may occur.",
                "preparation": "Exercise due safety when moving about.",
                "action": "Review your seasonal plan and ensure readiness for the next tropical tornado event.",
            },
        },
    },
}
PotentialImpactStatements = {
    "Wind": {
        "Extreme": ["Structural damage to sturdy buildings with some experiencing complete roof and wall failures. Complete destruction of mobile homes. Damage greatly accentuated by large airborne projectiles. Locations may be uninhabitable for weeks or months.",
                    "Numerous large trees snapped or uprooted along with fences and roadway signs blown over.",
                    "Many roads impassible from large debris, and more within urban or heavily wooded places. Many bridges, causeways, and access routes impassible.",
                    "Widespread power and communications outages."],
        "High": ["Considerable roof damage to sturdy buildings, with some having window, door, and garage door failures leading to structural damage. Mobile homes severely damaged, with some destroyed. Damage accentuated by airborne projectiles. Locations may be uninhabitable for weeks.",
                 "Many large trees snapped or uprooted along with fences and roadway signs blown over.",
                 "Some roads impassible from large debris, and more within urban or heavily wooded places. Several bridges, causeways, and access routes impassible.",
                 "Large areas with power and communications outages."],
        "Mod": ["Some damage to roofing and siding materials, along with damage to porches, awnings, carports, and sheds. A few buildings experiencing window, door, and garage door failures. Mobile homes damaged, especially if unanchored. Unsecured lightweight objects become dangerous projectiles.",
                "Several large trees snapped or uprooted, but with greater numbers in places where trees are shallow rooted. Several fences and roadway signs blown over.",
                "Some roads impassible from large debris, and more within urban or heavily wooded places. A few bridges, causeways, and access routes impassible.",
                "Scattered power and communications outages, but more prevalent in areas with above ground lines."],
        "Elevated": ["Damage to porches, awnings, carports, sheds, and unanchored mobile homes. Unsecured lightweight objects blown about.",
                     "Many large tree limbs broken off. A few trees snapped or uprooted, but with greater numbers in places where trees are shallow rooted. Some fences and roadway signs blown over.",
                     "A few roads impassible from debris, particularly within urban or heavily wooded places. Hazardous driving conditions on bridges and other elevated roadways.",
                     "Scattered power and communications outages."],
        "None": ["Little to no potential impacts from wind."],
    },
    "Storm Surge": {
        "Extreme": ["Widespread deep inundation, with storm surge flooding greatly accentuated by powerful battering waves. Structural damage to buildings, with many washing away. Damage greatly compounded from considerable floating debris. Locations may be uninhabitable for an extended period.",
                    "Near-shore escape routes and secondary roads washed out or severely flooded. Flood control systems and barriers may become stressed.",
                    "Extreme beach erosion. New shoreline cuts possible.",
                    "Massive damage to marinas, docks, boardwalks, and piers. Numerous small craft broken away from moorings with many lifted onshore and stranded."],
        "High": ["Large areas of deep inundation, with storm surge flooding accentuated by battering waves. Structural damage to buildings, with several washing away. Damage compounded by floating debris. Locations may be uninhabitable for an extended period.",
                 "Large sections of near-shore escape routes and secondary roads washed out or severely flooded. Flood control systems and barriers may become stressed.",
                 "Severe beach erosion with significant dune loss.",
                 "Major damage to marinas, docks, boardwalks, and piers. Many small craft broken away from moorings, especially in unprotected anchorages with some lifted onshore and stranded."],
        "Mod": ["Areas of inundation with storm surge flooding accentuated by waves. Damage to several buildings, mainly near the coast.",
                "Sections of near-shore escape routes and secondary roads become weakened or washed out, especially in usually vulnerable low spots.",
                "Major beach erosion with heavy surf breaching dunes. Strong and numerous rip currents.",
                "Moderate damage to marinas, docks, boardwalks, and piers. Several small craft broken away from moorings, especially in unprotected anchorages."],
        "Elevated": ["Localized inundation with storm surge flooding mainly along immediate shorelines and in low-lying spots, or in areas farther inland near where higher surge waters move ashore.",
                     "Sections of near-shore roads and parking lots become overspread with surge water. Driving conditions dangerous in places where surge water covers the road.",
                     "Moderate beach erosion. Heavy surf also breaching dunes, mainly in usually vulnerable locations. Strong rip currents.",
                     "Minor to locally moderate damage to marinas, docks, boardwalks, and piers. A few small craft broken away from moorings."],
        "None": ["Little to no potential impacts from storm surge flooding."],
    },
    "Flooding Rain": {
        "Extreme": ["Extreme rainfall flooding may prompt numerous evacuations and rescues.",
                    "Rivers and tributaries may overwhelmingly overflow their banks in many places with deep moving water. Small streams, creeks, canals, arroyos, and ditches may become raging rivers. In mountain areas, deadly runoff may rage down valleys while increasing susceptibility to rockslides and mudslides. Flood control systems and barriers may become stressed.",
                    "Flood waters can enter numerous structures within multiple communities, with some structures becoming uninhabitable or washed away. Numerous places where flood waters may cover escape routes. Streets and parking lots become rivers of raging water with underpasses submerged. Driving conditions become very dangerous. Numerous road and bridge closures with some weakened or washed out."],
        "High": ["Major rainfall flooding may prompt many evacuations and rescues.",
                 "Rivers and tributaries may rapidly overflow their banks in multiple places. Small streams, creeks, canals, arroyos, and ditches may become dangerous rivers. In mountain areas, destructive runoff may run quickly down valleys while increasing susceptibility to rockslides and mudslides. Flood control systems and barriers may become stressed.",
                 "Flood waters can enter many structures within multiple communities, with some structures becoming uninhabitable or washed away. Many places where flood waters may cover escape routes. Streets and parking lots become rivers of moving water with underpasses submerged. Driving conditions become dangerous. Many road and bridge closures with some weakened or washed out."],
        "Mod": ["Moderate rainfall flooding may prompt several evacuations and rescues.",
                "Rivers and tributaries may quickly become swollen with swifter currents and overspill their banks in a few places, especially in usually vulnerable spots. Small streams, creeks, canals, arroyos, and ditches overflow.",
                "Flood waters can enter some structures or weaken foundations. Several places may experience expanded areas of rapid inundation at underpasses, low-lying spots, and poor drainage areas. Some streets and parking lots take on moving water as storm drains and retention ponds overflow. Driving conditions become hazardous. Some road and bridge closures."],
        "Elevated": ["Localized rainfall flooding may prompt a few evacuations.",
                     "Rivers and tributaries may quickly rise with swifter currents. Small streams, creeks, canals, arroyos, and ditches may become swollen and overflow in spots.",
                     "Flood waters can enter a few structures, especially in usually vulnerable spots.  A few places where rapid ponding of water occurs at underpasses, low-lying spots, and poor drainage areas. Several storm drains and retention ponds may become near-full and begin to overflow. Some brief road and bridge closures."],
        "None": ["Little to no potential impacts from flooding rain."],
    },
    "Tornado": {
        "Extreme": ["The occurrence of an outbreak of tornadoes can greatly hinder the execution of emergency plans during tropical events.",
                    "Many places may experience tornado damage, with several spots of immense destruction, power loss, and communications failures.",
                    "Locations could realize sturdy buildings demolished, structures upon weak foundations swept away, mobile homes obliterated, large trees twisted and snapped with some debarked, vehicles lifted off the ground and thrown with distance, and small boats destroyed. Large and deadly projectiles can add considerably to the toll."],
        "High": ["The occurrence of numerous tornadoes can greatly hinder the execution of emergency plans during tropical events.",
                 "Many places may experience tornado damage with a few spots of immense destruction, power loss, and communications failures.",
                 "Locations could realize roof and wall failures of sturdy buildings with some being leveled, structures upon weak foundations blown away, mobile homes obliterated, large trees twisted and snapped with forested trees uprooted, vehicles lifted off the ground and thrown, and small boats destroyed. Large and deadly projectiles can add to the toll."],
        "Mod": ["The occurrence of scattered tornadoes can hinder the execution of emergency plans during tropical events.",
                "Several places may experience tornado damage with a few spots of considerable damage, power loss, and communications failures.",
                "Locations could realize roofs torn off frame houses, mobile homes demolished, boxcars overturned, large trees snapped or uprooted, vehicles tumbled, and small boats tossed about. Dangerous projectiles can add to the toll."],
        "Elevated": ["The occurrence of isolated tornadoes can hinder the execution of emergency plans during tropical events.",
                     "A few places may experience tornado damage, along with power and communications disruptions.",
                     "Locations could realize roofs peeled off buildings, chimneys toppled, mobile homes pushed off foundations or overturned, large tree tops and branches snapped off, shallow-rooted trees knocked over, moving vehicles blown off roads, and small boats pulled from moorings."],
        "None": ["Little to no potential impacts from tornadoes."],
    },
}

EvacuationStatements = ["For those under evacuation orders, leave as soon as practical with a destination in mind. Gas up your vehicle well ahead of time. Be sure that you take essential materiasl from your Emergency Supplies Kit. Let others know where you are going and when you intend to arrive.",
                        "If evacuating the area, stick to prescribed evacuation routes. Look for additional traffic information on roadway smart signs and listen to select radio channels for further travel instructions. Do not use your cell phone while driving."
                        "For those not under evacuation orders, understand that there are inherent risks to evacuation (such as traffic congestion, accidents, and driving in bad weather), so evacuate only if necessary. Help keep roadways open for those that are under evacuation orders."]

OtherPreparednessActions = {
                                                      
    "check plans": ["Now is the time to check your emergency plan and take necessary actions to secure your home or business. Deliberate efforts should be underway to protect life and property. Ensure that your Emergency Supplies Kit is stocked and ready.",
                    "When making safety and preparedness decisions, do not focus on the exact forecast track as there are inherent forecast uncertainties which must be taken into account.",
                    "If you live in a place that is particularly vulnerable to high wind, such as a mobile home, an upper floor of a high rise building, or on a boat, plan to move to safe shelter. Take enough supplies for you and your family for several days.",
                    "If you live in a place particularly vulnerable to flooding, such as near the ocean or a large inland lake, in a low lying or poor drainage area, in a valley or canyon, or near an already swollen river, plan to move to safe shelter on higher ground",
                    "Always heed the advice of local officials and comply with any orders that are issued. Do not needlessly jeopardize your life or the lives of others.",
                    "When securing your property, outside preparations should be conducted as soon as possible before conditions deteriorate. The onset of strong gusty winds and heavy rain can cause certain preparedness activities to become unsafe.",
                    "Be sure to let friends and other family members know of your intentions and whereabouts for surviving the storm. For emergency purposes, have someone located away from the threatened area serve as your point of contact. Share vital contact information with others. Keep cell phones handy and well charged.",
                    "Be a Good Samaritan and check on those who may not be fully aware of the situation or who are unable to make personal preparations.",
                    "Visitors to the area should become familiar with nearby surroundings. If you are a visitor, know the name of the county or parish in which you are located and where it is relative to current watches and warnings. If staying at a hotel, ask the management staff about their onsite disaster plan. Listen for evacuation orders, especially pertaining to area visitors.",
                    "Closely monitor NOAA Weather Radio or other local news outlets for official storm information. Listen for possible changes to the forecast."],
    "complete preparations": ["Now is the time to bring to completion all preparations to protect life and property in accordance with your emergency plan.",
                              "Outside preparations should be wrapped up as soon as possible before weather conditions completely deteriorate. Any remaining evacuations and relocations should be expedited before the onset of tropical storm force wind.",  
                              "If you are relocating to safe shelter, leave as early as possible. If heading to a community shelter, become familiar with the shelter rules before arrival, especially if you have special needs or own a pet. Take essential items with you from your Emergency Supplies Kit. Check the latest weather forecast before departing.", 
                              "Failure to adequately shelter may result in serious injury or loss of life. Always heed the advice of local officials and comply with any orders that are issued. Remember, during the storm 9 1 1 Emergency Services may not be able to immediately respond if conditions are unsafe. This should be a big factor in your decision making.",
                              "Check-in with your Emergency Points of Contact among family, friends, and workmates. Inform them of your status and well-being. Let them know how you intend to ride out the storm and when you plan to check-in again.",
                              "Keep cell phones well charged and handy. Also, cell phone chargers for automobiles can be helpful after the storm. Locate your chargers and keep them with your cell phone.",
                              "In emergencies it is best to remain calm. Stay informed and focused on the situation at hand. Exercise patience with those you encounter. Be a Good Samaritan and helpful to others.",                              
                              "If relocating to a nearby shelter or to the home of a family member or friend, drive with extra caution, especially on secondary roads. Remember, many bridges and causeways will be closed once higher winds arrive. Also, if you encounter water covering the road, seek an alternate route. Always obey official road signs for closures and detours.",
                              "If you are a visitor and still in the area, listen for the name of the city or town in which you are staying within local news updates. Be sure you know the name of the county or parish in which it resides. Pay attention for instructions from local authorities.",
                              "Closely monitor NOAA Weather radio or other local news outlets for official storm information. Be ready to adapt to possible changes to the forecast."],
    "hunker down": ["Now is the time to remain safely sheltered from the storm. Stay inside and away from windows. Listen for updates and be ready in case you lose electrical power. Locate your battery powered radio and flashlight from your Emergency Supplies Kit. Keep these items close.",
                    "During the peak of the storm, keep your shoes on and rain gear handy. Boots and tennis shoes offer the best foot protection if you become unexpectedly exposed to the elements.",
                    "Continue to keep your cell phone well charged for as long as possible. If you lose power, use it more sparingly and mainly for personal emergencies and check-ins. Do not overload communications systems with idle chatter.",
                    "Do not venture outside while in the eye of a hurricane. Within the eye, weather conditions may temporarily improve which can be misleading. Once the eye passes, the wind will change direction and return to dangerous speeds. Heavy rain will also return. Be smart and remain safely hidden from the storm.",
                    "Do not be a thrill seeker or risk your life for senseless photos or videos. Be wise and avoid becoming another statistic.",
                    "Be ready to move to the identified safe room if your home or shelter begins to fail. Quickly move to an interior room on the lowest floor. Put as many sturdy walls between you and the storm as you can. Protect your head and body.",
                    "When major hurricanes make landfall, extreme winds bring a tremendous threat to life and cause devastating to catastrophic damage. The extent of these extreme winds is usually confined to locations near the coast and does not tend to cover an overly large area. Yet, this area will realize the brunt of the storm. At the onset of landfall, listen for extreme wind warnings. If issued for you area, move to the safest place within your home or shelter. Take the same life-saving actions as if it were a violent tornado."],
    "recovery": ["Remain safely sheltered until the storm fully passes. Once conditions improve, be careful going outside. Stay away from downed power lines and hazardous debris.",
                 "If your home or shelter was damaged, be alert to the smell of natural gas leaks and cautious around exposed electrical wiring, broken glass, jagged metal and wood, and protruding nails and screws.",
                 "Check to see if everyone in your group is OK. Administer first aid to those who are injured. Call 9 1 1 for any serious injuries. Remember, it may be more difficult for emergency responders to arrive quickly in the time period immediately following the storm.", 
                 "Check-in with your Emergency Points of Contact. Let them know of your status and well-being. Keep conversations short and to the point. Do not tie up communications systems.",
                 "Be a good neighbor and check on those living next to you. Be neighborly and lend a helping hand.",
                 "Those who rode out the storm away from their home or business are likely anxious to return. However, allow some time for work crews to make a clear path for emergency vehicles. Downed power lines and trees may be blocking roads and flood waters may have washed out or overspread sections of key travel routes. Traffic lights may also be out of service.",
                 "Do not attempt to return to evacuated areas until local authorities give the All-Clear signal.",
                 "Do not go sightseeing within impacted communities simply to observe storm damage. Sightseers can interfere with the timeliness of rescuers and first responders while needlessly jeopardizing their own lives and the lives of others.",
                 "When inspecting damage, use flashlights rather than candles or flamed lighting. Be aware of sparks that can ignite leaking gas or other flammables.",
                 "Do not go up on your roof until the rain and strong winds have subsided. Ladders can be slippery in the rain and unexpected wind gusts can blow you off of the roof. Do not risk bodily harm in an attempt to reduce property damage.",
                 "When clearing out fallen trees, be careful with chain saws and axes. Always wear protective gear and keep others at a safe distance. Use these tools according to operating manuals and safety instruction. Leaning trees and those which have fallen on roof tops can be especially challenging. If you are not in good health or unsure about what you are doing, have someone else with tree cutting experience do the job. Never cut trees without a partner.",
                 "If using a generator, avoid carbon monoxide poisoning by following instructions by the manufacturer. Make sure that the generator is run in a well ventilated space.",
                 "Problems with sewer backups can contaminate standing flood waters. Keep children away. Also, listen for boil water alerts relative to communities whose tap water may have become temporarily non-potable."],
}

AdditionalSources = ["- For information on appropriate preparations see ready.gov/louisiana",
                     "- For information on local evacuation shelters see www.emergency.louisana.gov/disaster_evaluation_guide.html",
                     "- For information on creating an emergency plan see getagameplan.org",
                     "- For additional disaster preparedness information see redcross.org"]
