# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# TCV_Dictionary
#   TCV_Dictionary file
# Author: Shannon White/Pablo Santos/David Sharp
# Last Modified: November 29, 2017
#
# 11/29/2017 by swhite to modify P/P statements based on field input
# 01/31/2018 Replace non-ASCII chars (DR #20554)
# ----------------------------------------------------------------------------
#  Needed to prevent an error from the SmartTool module

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

WeatherElementEdited = None

ThreatStatements = {
    "Wind": {
        "Extreme": {
            "check plans": {
                "planning": "PLAN: Plan for extreme wind of equivalent CAT 3 hurricane force or higher.",
                "preparation": "PREPARE: Efforts to protect life and property should now be underway. Prepare for catastrophic wind damage.",
                "action": "ACT: Act now to complete preparations before the wind becomes hazardous.",
            },
            "complete preparations": {
                "planning": "PLAN: Plan for extreme wind of equivalent CAT 3 hurricane force or higher.",
                "preparation": "PREPARE: Remaining efforts to protect life and property should be urgently completed. Prepare for catastrophic wind damage.",
                "action": "ACT: Move to safe shelter before the wind becomes hazardous.",
            },
            "hunker down": {
                "planning": "PLAN: Plan for extreme wind of equivalent CAT 3 hurricane force or higher.",
                "preparation": "PREPARE: Last minute efforts should solely focus on protecting life. The area remains subject to catastrophic wind damage.",
                "action": "ACT: Now is the time to shelter from life-threatening wind. Be ready to move to the safest place inside your shelter if necessary.",
            },
            "recovery": {
                "planning": "PLAN: The threat of hazardous wind has subsided.",
                "preparation": "PREPARE: Heed instructions from local officials when moving about. Avoid restricted areas.",
                "action": "ACT: Dial 9 1 1 if you have a life-threatening emergency. Practice safety when moving about.",
            },
            "default": {
                "planning": "PLAN: Plan for extreme wind of equivalent CAT 3 hurricane force or higher.",
                "preparation": "PREPARE: Efforts to protect life and property should now be underway. Prepare for catastrophic wind damage.",
                "action": "ACT: Act now to complete preparations before the wind becomes hazardous.",
            },
        },
        "High": {
            "check plans": {
                "planning": "PLAN: Plan for life-threatening wind of equivalent CAT 1 or 2 hurricane force.",
                "preparation": "PREPARE: Efforts to protect life and property should now be rigorously underway. Prepare for considerable wind damage.",
                "action": "ACT: Act now to complete preparations before the wind becomes hazardous.",
            },
            "complete preparations": {
                "planning": "PLAN: Plan for life-threatening wind of equivalent CAT 1 or 2 hurricane force.",
                "preparation": "PREPARE: Remaining efforts to protect life and property should be urgently completed. Prepare for considerable wind damage.",
                "action": "ACT: Move to safe shelter before the wind becomes hazardous.",
            },
            "hunker down": {
                "planning": "PLAN: Plan for life-threatening wind of equivalent CAT 1 or 2 hurricane force.",
                "preparation": "PREPARE: Last minute efforts should solely focus on protecting life. The area remains subject to considerable wind damage.",
                "action": "ACT: Now is the time to shelter from life-threatening wind.",
            },
            "recovery": {
                "planning": "PLAN: The threat of hazardous wind has subsided.",
                "preparation": "PREPARE: Heed instructions from local officials when moving about. Avoid restricted areas.",
                "action": "ACT: Dial 9 1 1 if you have a life-threatening emergency. Practice safety when moving about.",
            },
            "default": {
                "planning": "PLAN: Plan for life-threatening wind of equivalent CAT 1 or 2 hurricane force.",
                "preparation": "PREPARE: Efforts to protect life and property should now be rigorously underway. Prepare for considerable wind damage.",
                "action": "ACT: Act now to complete preparations before the wind becomes hazardous.",
            },
        },
        "Mod": {
            "check plans": {
                "planning": "PLAN: Plan for dangerous wind of equivalent strong tropical storm force.",
                "preparation": "PREPARE: Efforts to protect life and property should now be underway. Prepare for significant wind damage.",
                "action": "ACT: Act now to complete preparations before the wind becomes hazardous.",
            },
            "complete preparations": {
                "planning": "PLAN: Plan for dangerous wind of equivalent strong tropical storm force.",
                "preparation": "PREPARE: Remaining efforts to protect life and property should be completed as soon as possible. Prepare for significant wind damage.",
                "action": "ACT: Move to safe shelter before the wind becomes hazardous.",
            },
            "hunker down": {
                "planning": "PLAN: Plan for dangerous wind of equivalent strong tropical storm force.",
                "preparation": "PREPARE: Last minute efforts to protect life and property should now be complete. The area remains subject to significant wind damage.",
                "action": "ACT: Now is the time to shelter from dangerous wind.",
            },
            "recovery": {
                "planning": "PLAN: The threat of hazardous wind has subsided.",
                "preparation": "PREPARE: Heed instructions from local officials when moving about. Avoid restricted areas.",
                "action": "ACT: Dial 9 1 1 if you have a life-threatening emergency. Practice safety when moving about.",
            },
            "default": {
                "planning": "PLAN: Plan for dangerous wind of equivalent strong tropical storm force.",
                "preparation": "PREPARE: Efforts to protect life and property should now be underway. Prepare for significant wind damage.",
                "action": "ACT: Act now to complete preparations before the wind becomes hazardous.",
            },
        },
        "Elevated": {
            "check plans": {
                "planning": "PLAN: Plan for hazardous wind of equivalent tropical storm force.",
                "preparation": "PREPARE: Efforts to protect property should now be underway. Prepare for limited wind damage.",
                "action": "ACT: Act now to complete preparations before the wind becomes hazardous.",
            },
            "complete preparations": {
                "planning": "PLAN: Plan for hazardous wind of equivalent tropical storm force.",
                "preparation": "PREPARE: Remaining efforts to protect property should be completed as soon as possible. Prepare for limited wind damage.",
                "action": "ACT: Move to safe shelter before the wind becomes hazardous.",
            },
            "hunker down": {
                "planning": "PLAN: Plan for hazardous wind of equivalent tropical storm force.",
                "preparation": "PREPARE: Last minute efforts to protect property should now be complete. The area remains subject to limited wind damage.",
                "action": "ACT: Now is the time to shelter from hazardous wind.",
            },
            "recovery": {
                "planning": "PLAN: The threat of hazardous wind has subsided.",
                "preparation": "PREPARE: Heed instructions from local officials when moving about. Avoid restricted areas.",
                "action": "ACT: Dial 9 1 1 if you have a life-threatening emergency. Practice safety when moving about.",
            },
            "default": {
                "planning": "PLAN: Plan for hazardous wind of equivalent tropical storm force.",
                "preparation": "PREPARE: Efforts to protect property should now be underway. Prepare for limited wind damage.",
                "action": "ACT: Act now to complete preparations before the wind becomes hazardous.",
            },
        },
        "None": {
            "check plans": {
                "planning": "PLAN: The sustained wind should remain less than tropical storm force. Conditions may still be gusty.",
                "preparation": "PREPARE: Little to no preparations needed to guard against tropical winds at this time.",
                "action": "ACT: Ensure emergency readiness should the forecast change.",
            },
            "complete preparations": {
                "planning": "PLAN: The sustained wind should remain less than tropical storm force. Conditions may still be gusty.",
                "preparation": "PREPARE: Little to no preparations needed to guard against tropical winds at this time.",
                "action": "ACT: Ensure emergency readiness should the forecast change.",
            },
            "hunker down": {
                "planning": "PLAN: The sustained wind should remain less than tropical storm force. Conditions may still be gusty.",
                "preparation": "PREPARE: Little to no preparations needed to guard against tropical winds at this time.",
                "action": "ACT: Ensure emergency readiness should the forecast change.",
            },
            "recovery": {
                "planning": "PLAN: The sustained wind should remain less than tropical storm force. Conditions may still be gusty.",
                "preparation": "PREPARE: Listen for any instructions from local officials.",
                "action": "ACT: Ensure emergency readiness should the forecast change.",
            },
            "default": {
                "planning": "PLAN: The sustained wind should remain less than tropical storm force. Conditions may still be gusty.",
                "preparation": "PREPARE: Little to no preparations needed to guard against tropical winds at this time.",
                "action": "ACT: Ensure emergency readiness should the forecast change.",
            },
        },
    },
    "Storm Surge": {
        "Extreme": {
            "check plans": {
                "planning": "PLAN: Plan for extreme life-threatening storm surge flooding greater than 9 feet above ground.",
                "preparation": "PREPARE: Evacuation preparations should be underway. Assemble disaster supplies and know your evacuation route.",
                "action": "ACT: Leave if evacuation orders are given for your area. Failure to heed evacuation orders may result in the loss of your life.",
            },
            "complete preparations": {
                "planning": "PLAN: Plan for extreme life-threatening storm surge flooding greater than 9 feet above ground.",
                "preparation": "PREPARE: Evacuation efforts should soon be brought to completion before driving conditions become unsafe.",
                "action": "ACT: Leave immediately if evacuation orders are given for your area to avoid being cut off from emergency services or needlessly risk lives.",
            },
            "hunker down": {
                "planning": "PLAN: Shelter against extreme life-threatening storm surge flooding greater than 9 feet above ground.",
                "preparation": "PREPARE: All ordered evacuations should be complete. Evacuees should be in shelters well away from storm surge flooding.",
                "action": "ACT: Remain sheltered in a safe location. Do not venture outside. Move to upper floors to escape rising water if necessary.",
            },
            "recovery": {
                "planning": "PLAN: The threat of life-threatening storm surge is diminishing as flood waters recede.",
                "preparation": "PREPARE: Do not return to evacuated areas until flood waters completely recede and the all-clear is given by local officials.",
                "action": "ACT: Failure to practice safety may result in serious injury or loss of life. If you have a life-threatening emergency, dial 9 1 1.",
            },
            "default": {
                "planning": "PLAN: Plan for extreme life-threatening storm surge flooding greater than 9 feet above ground.",
                "preparation": "PREPARE: Evacuation preparations should be underway. Assemble disaster supplies and know your evacuation route.",
                "action": "ACT: Leave if evacuation orders are given for your area. Failure to heed evacuation orders may result in the loss of your life.",
            },
        },
        "High": {
            "check plans": {
                "planning": "PLAN: Plan for life-threatening storm surge flooding of greater than 6 feet above ground.",
                "preparation": "PREPARE: Evacuation preparations should be underway. Assemble disaster supplies and know your evacuation route.",
                "action": "ACT: Leave if evacuation orders are given for your area. Failure to heed evacuation orders may result in the loss of your life.",
            },
            "complete preparations": {
                "planning": "PLAN: Plan for life-threatening storm surge flooding of greater than 6 feet above ground.",
                "preparation": "PREPARE: Evacuation efforts should soon be brought to completion before driving conditions become unsafe.",
                "action": "ACT: Leave immediately if evacuation orders are given for your area to avoid being cut off from emergency services or needlessly risk lives.",
            },
            "hunker down": {
                "planning": "PLAN: Shelter against life-threatening storm surge flooding of greater than 6 feet above ground.",
                "preparation": "PREPARE: All ordered evacuations should be complete. Evacuees should be in shelters well away from storm surge flooding.",
                "action": "ACT: Remain sheltered in a safe location. Do not venture outside. Move to upper floors to escape rising water if necessary.",
            },
            "recovery": {
                "planning": "PLAN: The threat of life-threatening storm surge is diminishing as flood waters recede.",
                "preparation": "PREPARE: Do not return to evacuated areas until flood waters completely recede and the all-clear is given by local officials.",
                "action": "ACT: Failure to practice safety may result in serious injury or loss of life. If you have a life-threatening emergency, dial 9 1 1.",
            },
            "default": {
                "planning": "PLAN: Plan for life-threatening storm surge flooding of greater than 6 feet above ground.",
                "preparation": "PREPARE: Evacuation preparations should be underway. Assemble disaster supplies and know your evacuation route.",
                "action": "ACT: Leave if evacuation orders are given for your area. Failure to heed evacuation orders may result in the loss of your life.",
            },
        },
        "Mod": {
            "check plans": {
                "planning": "PLAN: Plan for life-threatening storm surge flooding of greater than 3 feet above ground.",
                "preparation": "PREPARE: Storm surge flooding preparations should be underway. Assemble disaster supplies and know your evacuation route.",
                "action": "ACT: Leave if evacuation orders are given for your area. Failure to heed evacuation orders may result in the loss of your life.",
            },
            "complete preparations": {
                "planning": "PLAN: Plan for life-threatening storm surge flooding of greater than 3 feet above ground.",
                "preparation": "PREPARE: Evacuation efforts and flood preparations should soon be brought to completion before conditions become unsafe.",
                "action": "ACT: Leave immediately if evacuation orders are given for your area to avoid being cut off from emergency services or needlessly risk lives.",
            },
            "hunker down": {
                "planning": "PLAN: Shelter against life-threatening storm surge of greater than 3 feet above ground.",
                "preparation": "PREPARE: Flood preparations and ordered evacuations should be complete. Evacuees should be in shelters well away from storm surge flooding.",
                "action": "ACT: Remain sheltered in a safe location. Do not venture outside.",
            },
            "recovery": {
                "planning": "PLAN: The threat of life-threatening storm surge is diminishing as flood waters recede.",
                "preparation": "PREPARE: Do not return to evacuated areas until flood waters completely recede and the all-clear is given by local officials.",
                "action": "ACT: Failure to practice safety may result in serious injury or loss of life. If you have a life-threatening emergency, dial 9 1 1.",
            },
            "default": {
                "planning": "PLAN: Plan for life-threatening storm surge flooding of greater than 3 feet above ground.",
                "preparation": "PREPARE: Storm surge flooding preparations should be underway. Assemble disaster supplies and know your evacuation route.",
                "action": "ACT: Leave if evacuation orders are given for your area. Failure to heed evacuation orders may result in the loss of your life.",
            },
        },
        "Elevated": {
            "check plans": {
                "planning": "PLAN: Plan for storm surge flooding greater than 1 foot above ground.",
                "preparation": "PREPARE: Efforts should now be underway to prepare for storm surge flooding, especially in low-lying vulnerable areas.",
                "action": "ACT: Take actions to protect life and property. Prepare to leave if evacuation orders are given for your area.",
            },
            "complete preparations": {
                "planning": "PLAN: Plan for storm surge flooding greater than 1 foot above ground.",
                "preparation": "PREPARE: Complete preparations for storm surge flooding, especially in low-lying vulnerable areas, before conditions become unsafe.",
                "action": "ACT: Leave immediately if evacuation orders are given for your area.",
            },
            "hunker down": {
                "planning": "PLAN: Shelter against storm surge flooding greater than 1 foot above ground.",
                "preparation": "PREPARE: All flood preparations should be complete. Expect flooding of low-lying roads and property.",
                "action": "ACT: Stay away from storm surge prone areas. Continue to follow the instructions of local officials.",
            },
            "recovery": {
                "planning": "PLAN: The threat from storm surge is diminishing as flood waters recede.",
                "preparation": "PREPARE: Heed instructions from local officials when moving about. Do not enter flooded areas.",
                "action": "ACT: Exercise safety.",
            },
            "default": {
                "planning": "PLAN: Plan for storm surge flooding greater than 1 foot above ground.",
                "preparation": "PREPARE: Efforts should now be underway to prepare for storm surge flooding, especially in low-lying vulnerable areas.",
                "action": "ACT: Take actions to protect life and property. Prepare to leave if evacuation orders are given for your area.",
            },
        },
        "None": {
            "check plans": {
                "planning": "PLAN: There is little to no threat of storm surge flooding. Rough surf, coastal erosion, and life-threatening rip currents are possible.",
                "preparation": "PREPARE: Little to no preparations for storm surge flooding are needed.",
                "action": "ACT: Follow the instructions of local officials. Monitor forecasts.",
            },
            "complete preparations": {
                "planning": "PLAN: There is little to no threat of storm surge flooding. Rough surf, coastal erosion, and life-threatening rip currents are possible.",
                "preparation": "PREPARE: Little to no preparations for storm surge flooding are needed.",
                "action": "ACT: Follow the instructions of local officials. Monitor forecasts.",
            },
            "hunker down": {
                "planning": "PLAN: There is little to no threat of storm surge flooding. Rough surf, coastal erosion, and life-threatening rip currents are possible.",
                "preparation": "PREPARE: Little to no preparations for storm surge flooding are needed.",
                "action": "ACT: Follow the instructions of local officials. Monitor forecasts.",
            },
            "recovery": {
                "planning": "PLAN: There is little to no threat of storm surge flooding. Rough surf, coastal erosion, and life-threatening rip currents are possible.",
                "preparation": "PREPARE: Little to no preparations for storm surge flooding are needed.",
                "action": "ACT: Follow the instructions of local officials. Monitor forecasts.",
            },
            "default": {
                "planning": "PLAN: There is little to no threat of storm surge flooding. Rough surf, coastal erosion, and life-threatening rip currents are possible.",
                "preparation": "PREPARE: Little to no preparations for storm surge flooding are needed.",
                "action": "ACT: Follow the instructions of local officials. Monitor forecasts.",
            },
        },
    },
    "Flooding Rain": {
        "Extreme": {
            "check plans": {
                "planning": "PLAN: Emergency plans should include the potential for extreme flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Urgently consider protective actions from extreme and widespread rainfall flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
            "complete preparations": {
                "planning": "PLAN: Emergency plans should include the potential for extreme flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Urgently consider protective actions from extreme and widespread rainfall flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
            "hunker down": {
                "planning": "PLAN: Emergency plans should include the potential for extreme flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Urgently consider protective actions from extreme and widespread rainfall flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
            "recovery": {
                "planning": "PLAN: Emergency plans should include the potential for extreme flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Urgently consider protective actions from extreme and widespread rainfall flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
            "default": {
                "planning": "PLAN: Emergency plans should include the potential for extreme flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Urgently consider protective actions from extreme and widespread rainfall flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
        },
        "High": {
            "check plans": {
                "planning": "PLAN: Emergency plans should include the potential for major flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Strongly consider protective actions, especially if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
            "complete preparations": {
                "planning": "PLAN: Emergency plans should include the potential for major flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Strongly consider protective actions, especially if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
            "hunker down": {
                "planning": "PLAN: Emergency plans should include the potential for major flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Strongly consider protective actions, especially if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
            "recovery": {
                "planning": "PLAN: Emergency plans should include the potential for major flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Strongly consider protective actions, especially if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
            "default": {
                "planning": "PLAN: Emergency plans should include the potential for major flooding from heavy rain. Evacuations and rescues are likely.",
                "preparation": "PREPARE: Strongly consider protective actions, especially if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings. Failure to take action will likely result in serious injury or loss of life.",
            },
        },
        "Mod": {
            "check plans": {
                "planning": "PLAN: Emergency plans should include the potential for moderate flooding from heavy rain. Evacuations and rescues are possible.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.  Failure to take action may result in serious injury or loss of life.",
            },
            "complete preparations": {
                "planning": "PLAN: Emergency plans should include the potential for moderate flooding from heavy rain. Evacuations and rescues are possible.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.  Failure to take action may result in serious injury or loss of life.",
            },
            "hunker down": {
                "planning": "PLAN: Emergency plans should include the potential for moderate flooding from heavy rain. Evacuations and rescues are possible.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.  Failure to take action may result in serious injury or loss of life.",
            },
            "recovery": {
                "planning": "PLAN: Emergency plans should include the potential for moderate flooding from heavy rain. Evacuations and rescues are possible.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.  Failure to take action may result in serious injury or loss of life.",
            },
            "default": {
                "planning": "PLAN: Emergency plans should include the potential for moderate flooding from heavy rain. Evacuations and rescues are possible.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.  Failure to take action may result in serious injury or loss of life.",
            },
        },
        "Elevated": {
            "check plans": {
                "planning": "PLAN: Emergency plans should include the potential for localized flooding from heavy rain.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.",
            },
            "complete preparations": {
                "planning": "PLAN: Emergency plans should include the potential for localized flooding from heavy rain.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.",
            },
            "hunker down": {
                "planning": "PLAN: Emergency plans should include the potential for localized flooding from heavy rain.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.",
            },
            "recovery": {
                "planning": "PLAN: Emergency plans should include the potential for localized flooding from heavy rain.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.",
            },
            "default": {
                "planning": "PLAN: Emergency plans should include the potential for localized flooding from heavy rain.",
                "preparation": "PREPARE: Consider protective actions if you are in an area vulnerable to flooding.",
                "action": "ACT: Heed any flood watches and warnings.",
            },
        },
        "None": {
            "check plans": {
                "planning": "PLAN: There is little or no potential for flooding rain.",
                "preparation": "PREPARE: Little to no preparations are needed to protect against flooding rain at this time.",
                "action": "ACT: Monitor for changes to the forecast.",
            },
            "complete preparations": {
                "planning": "PLAN: There is little or no potential for flooding rain.",
                "preparation": "PREPARE: Little to no preparations are needed to protect against flooding rain at this time.",
                "action": "ACT: Monitor for changes to the forecast.",
            },
            "hunker down": {
                "planning": "PLAN: There is little or no potential for flooding rain.",
                "preparation": "PREPARE: Little to no preparations are needed to protect against flooding rain at this time.",
                "action": "ACT: Monitor for changes to the forecast.",
            },
            "recovery": {
                "planning": "PLAN: There is little or no potential for flooding rain.",
                "preparation": "PREPARE: Little to no preparations are needed to protect against flooding rain at this time.",
                "action": "ACT: Monitor for changes to the forecast.",
            },
            "default": {
                "planning": "PLAN: There is little or no potential for flooding rain.",
                "preparation": "PREPARE: Little to no preparations are needed to protect against flooding rain at this time.",
                "action": "ACT: Monitor for changes to the forecast.",
            },
        },
    },
    "Tornado": {
        "Extreme": {
            "check plans": {
                "planning": "PLAN: Emergency plans should include the potential for an outbreak of many tornadoes with some possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats should prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
            "complete preparations": {
                "planning": "PLAN: Emergency plans should include the potential for an outbreak of many tornadoes with some possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats are urged to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
            "hunker down": {
                "planning": "PLAN: Emergency plans should continue to include the potential for an outbreak of tornadoes.",
                "preparation": "PREPARE: Stay within your shelter keeping informed of the latest tornado situation.",
                "action": "ACT: Move quickly to the safest place within your shelter if a tornado warning is issued.",
            },
            "recovery": {
                "planning": "PLAN: Plans should still include the potential for an outbreak of tornadoes.",
                "preparation": "PREPARE: Keep informed should additional weather alerts be needed.",
                "action": "ACT: If a tornado warning is issued, be ready to shelter quickly.",
            },
            "default": {
                "planning": "PLAN: Emergency plans should include the potential for an outbreak of many tornadoes with some possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats should prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
        },
        "High": {
            "check plans": {
                "planning": "PLAN: Emergency plans should include the potential for many tornadoes with some possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats should prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
            "complete preparations": {
                "planning": "PLAN: Emergency plans should include the potential for many tornadoes with some possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats are urged to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
            "hunker down": {
                "planning": "PLAN: Emergency plans should continue to include the potential for many tornadoes.",
                "preparation": "PREPARE: Stay within your shelter keeping informed of the latest tornado situation.",
                "action": "ACT: Move quickly to the safest place within your shelter if a tornado warning is issued.",
            },
            "recovery": {
                "planning": "PLAN: Plans should still include the potential for many tornadoes.",
                "preparation": "PREPARE: Keep informed should additional weather alerts be needed.",
                "action": "ACT: If a tornado warning is issued, be ready to shelter quickly.",
            },
            "default": {
                "planning": "PLAN: Emergency plans should include the potential for many tornadoes with some possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats should prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
        },
        "Mod": {
            "check plans": {
                "planning": "PLAN: Emergency plans should include the potential for several tornadoes with a few possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats should prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
            "complete preparations": {
                "planning": "PLAN: Emergency plans should include the potential for several tornadoes with a few  possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats are urged to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
            "hunker down": {
                "planning": "PLAN: Emergency plans should continue to include the potential for several tornadoes.",
                "preparation": "PREPARE: Stay within your shelter keeping informed of the latest tornado situation.",
                "action": "ACT: Move quickly to the safest place within your shelter if a tornado warning is issued.",
            },
            "recovery": {
                "planning": "PLAN: Plans should still include the potential for several tornadoes.",
                "preparation": "PREPARE: Keep informed should additional weather alerts be needed.",
                "action": "ACT: If a tornado warning is issued, be ready to shelter quickly.",
            },
            "default": {
                "planning": "PLAN: Emergency plans should include the potential for several tornadoes with a few possibly intense having larger damage paths.",
                "preparation": "PREPARE: Those living in manufactured homes or on boats should prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: Listen for tornado watches and warnings. If a tornado warning is issued, be ready to shelter quickly.",
            },
        },
        "Elevated": {
            "check plans": {
                "planning": "PLAN: Emergency plans should include the potential for a few tornadoes.",
                "preparation": "PREPARE: If your shelter is particularly vulnerable to tornadoes, prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: If a tornado warning is issued, be ready to shelter quickly.",
            },
            "complete preparations": {
                "planning": "PLAN: Emergency plans should include the potential for a few tornadoes.",
                "preparation": "PREPARE: If your shelter is particularly vulnerable to tornadoes, prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: If a tornado warning is issued, be ready to shelter quickly.",
            },
            "hunker down": {
                "planning": "PLAN: Emergency plans should continue to include possible tornadoes.",
                "preparation": "PREPARE: Stay within your shelter keeping informed of the latest tornado situation.",
                "action": "ACT: Move quickly to the safest place within your shelter if a tornado warning is issued.",
            },
            "recovery": {
                "planning": "PLAN: Plans should still include the potential for a few tornadoes.",
                "preparation": "PREPARE: Keep informed should additional weather alerts be needed.",
                "action": "ACT: If a tornado warning is issued, be ready to shelter quickly.",
            },
            "default": {
                "planning": "PLAN: Emergency plans should include the potential for a few tornadoes.",
                "preparation": "PREPARE: If your shelter is particularly vulnerable to tornadoes, prepare to relocate to safe shelter before hazardous weather arrives.",
                "action": "ACT: If a tornado warning is issued, be ready to shelter quickly.",
            },
        },
        "None": {
            "check plans": {
                "planning": "PLAN: Tornadoes are not expected. Showers and thunderstorms with gusty winds may still occur.",
                "preparation": "PREPARE: Little to no preparations needed to protect against tornadoes at this time. Keep informed of the latest tornado situation.",
                "action": "ACT: Listen for changes in the forecast.",
            },
            "complete preparations": {
                "planning": "PLAN: Tornadoes are not expected. Showers and thunderstorms with gusty winds may still occur.",
                "preparation": "PREPARE: Little to no preparations needed to protect against tornadoes at this time. Keep informed of the latest tornado situation.",
                "action": "ACT: Listen for changes in the forecast.",
            },
            "hunker down": {
                "planning": "PLAN: Tornadoes are not expected. Showers and thunderstorms with gusty winds may still occur.",
                "preparation": "PREPARE: Little to no preparations needed to protect against tornadoes at this time. Keep informed of the latest tornado situation.",
                "action": "ACT: Listen for changes in the forecast.",
            },
            "recovery": {
                "planning": "PLAN: Tornadoes are not expected. Showers and thunderstorms with gusty winds may still occur.",
                "preparation": "PREPARE: Little to no preparations needed to protect against tornadoes at this time. Keep informed of the latest tornado situation.",
                "action": "ACT: Listen for changes in the forecast.",
            },
            "default": {
                "planning": "PLAN: Tornadoes are not expected. Showers and thunderstorms with gusty winds may still occur.",
                "preparation": "PREPARE: Little to no preparations needed to protect against tornadoes at this time. Keep informed of the latest tornado situation.",
                "action": "ACT: Listen for changes in the forecast.",
            },
        },
    },
}
PotentialImpactStatements = {
    "Wind": {
        "Extreme": ["Structural damage to sturdy buildings, some with complete roof and wall failures. Complete destruction of mobile homes. Damage greatly accentuated by large airborne projectiles. Locations may be uninhabitable for weeks or months.",
                    "Numerous large trees snapped or uprooted along with fences and roadway signs blown over.",
                    "Many roads impassable from large debris, and more within urban or heavily wooded places. Many bridges, causeways, and access routes impassable.",
                    "Widespread power and communications outages."],
        "High": ["Considerable roof damage to sturdy buildings, with some having window, door, and garage door failures leading to structural damage. Mobile homes severely damaged, with some destroyed.  Damage accentuated by airborne projectiles.  Locations may be uninhabitable for weeks.",
                 "Many large trees snapped or uprooted along with fences and roadway signs blown over.",
                 "Some roads impassable from large debris, and more within urban or heavily wooded places. Several bridges, causeways, and access routes impassable.",
                 "Large areas with power and communications outages."],
        "Mod": ["Some damage to roofing and siding materials, along with damage to porches, awnings, carports, and sheds. A few buildings experiencing window, door, and garage door failures. Mobile homes damaged, especially if unanchored. Unsecured lightweight objects become dangerous projectiles.",
                "Several large trees snapped or uprooted, but with greater numbers in places where trees are shallow rooted. Several fences and roadway signs blown over.",
                "Some roads impassable from large debris, and more within urban or heavily wooded places. A few bridges, causeways, and access routes impassable.",
                "Scattered power and communications outages, but more prevalent in areas with above ground lines."],
        "Elevated": ["Damage to porches, awnings, carports, sheds, and unanchored mobile homes. Unsecured lightweight objects blown about.",
                     "Many large tree limbs broken off. A few trees snapped or uprooted, but with greater numbers in places where trees are shallow rooted. Some fences and roadway signs blown over.",
                     "A few roads impassable from debris, particularly within urban or heavily wooded places. Hazardous driving conditions on bridges and other elevated roadways.",
                     "Scattered power and communications outages."],
        "None": ["Little to no potential impacts from wind."],
    },
    "Storm Surge": {
        "Extreme": ["Widespread deep inundation, with storm surge flooding greatly accentuated by powerful battering waves. Structural damage to buildings, with many washing away. Damage greatly compounded from considerable floating debris. Locations may be uninhabitable for an extended period.",
                    "Near-shore escape routes and secondary roads washed out or severely flooded. Flood control systems and barriers may become stressed.",
                    "Extreme beach erosion. New shoreline cuts possible.",
                    "Massive damage to marinas, docks, boardwalks, and piers. Numerous small craft broken away from moorings with many lifted onshore and stranded."],
        "High": ["Large areas of deep inundation with storm surge flooding accentuated by battering waves. Structural damage to buildings, with several washing away. Damage compounded by floating debris. Locations may be uninhabitable for an extended period.",
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
                    "Flood waters can enter numerous structures within multiple communities, some structures becoming uninhabitable or washed away. Numerous places where flood waters may cover escape routes. Streets and parking lots become rivers of raging water with underpasses submerged. Driving conditions become very dangerous. Numerous road and bridge closures with some weakened or washed out."],
        "High": ["Major rainfall flooding may prompt many evacuations and rescues.",
                 "Rivers and tributaries may rapidly overflow their banks in multiple places. Small streams, creeks, canals, arroyos, and ditches may become dangerous rivers. In mountain areas, destructive runoff may run quickly down valleys while increasing susceptibility to rockslides and mudslides. Flood control systems and barriers may become stressed.",
                 "Flood waters can enter many structures within multiple communities, some structures becoming uninhabitable or washed away. Many places where flood waters may cover escape routes. Streets and parking lots become rivers of moving water with underpasses submerged. Driving conditions become dangerous. Many road and bridge closures with some weakened or washed out."],
        "Mod": ["Moderate rainfall flooding may prompt several evacuations and rescues.",
                "Rivers and tributaries may quickly become swollen with swifter currents and overspill their banks in a few places, especially in usually vulnerable spots. Small streams, creeks, canals, arroyos, and ditches overflow.",
                "Flood waters can enter some structures or weaken foundations. Several places may experience expanded areas of rapid inundation at underpasses, low-lying spots, and poor drainage areas. Some streets and parking lots take on moving water as storm drains and retention ponds overflow. Driving conditions become hazardous. Some road and bridge closures."],
        "Elevated": ["Localized rainfall flooding may prompt a few evacuations.",
                     "Rivers and tributaries may quickly rise with swifter currents. Small streams, creeks, canals, arroyos, and ditches may become swollen and overflow in spots.",
                     "Flood waters can enter a few structures, especially in usually vulnerable spots.  A few places where rapid ponding of water occurs at underpasses, low-lying spots, and poor drainage areas. Several storm drains and retention ponds become near-full and begin to overflow. Some brief road and bridge closures."],
        "None": ["Little to no potential impacts from flooding rain."],
    },
    "Tornado": {
        "Extreme": ["The occurrence of an outbreak of tornadoes can greatly hinder the execution of emergency plans during tropical events. ",
                    "Many places may experience tornado damage, with several spots of immense destruction, power loss, and communications failures.",
                    "Locations could realize sturdy buildings demolished, structures upon weak foundations swept away, mobile homes obliterated, large trees twisted and snapped with some debarked, vehicles lifted off the ground and thrown with distance, and boats destroyed. Large and deadly projectiles can add considerably to the toll."],
        "High": ["The occurrence of numerous tornadoes can greatly hinder the execution of emergency plans during tropical events.",
                 "Many places may experience tornado damage with a few spots of immense destruction, power loss, and communications failures.",
                 "Locations could realize roof and wall failures of sturdy buildings with some being leveled, structures upon weak foundations blown away, mobile homes obliterated, large trees twisted and snapped with forested trees uprooted, vehicles lifted off the ground and thrown, and boats destroyed. Large and deadly projectiles can add to the toll."],
        "Mod": ["The occurrence of scattered tornadoes can hinder the execution of emergency plans during tropical events.",
                "Several places may experience tornado damage with a few spots of considerable damage, power loss, and communications failures.",
                "Locations could realize roofs torn off frame houses, mobile homes demolished, boxcars overturned, large trees snapped or uprooted, vehicles tumbled, and boats tossed about. Dangerous projectiles can add to the toll."],
        "Elevated": ["The occurrence of isolated tornadoes can hinder the execution of emergency plans during tropical events.",
                     "A few places may experience tornado damage, along with power and communications disruptions.",
                     "Locations could realize roofs peeled off buildings, chimneys toppled, mobile homes pushed off foundations or overturned, large tree tops and branches snapped off, shallow-rooted trees knocked over, moving vehicles blown off roads, and boats pulled from moorings."],
        "None": ["Little to no potential impacts from tornadoes."],
    },
}

EvacuationStatements = [
    "WATCH/WARNING PHASE - Listen to local official for recommended preparedness actions, including possible evacuation.  If ordered to evacuate, do so immediately.",
    "WATCH/WARNING PHASE - For those not under evacuation orders, assess the risk from wind, falling trees, and flooding at your location.  If you decide to move, relocate to a safer location nearby.  If you do not relocate, help keep roadways open for those under evacuation orders.",
    "WATCH/WARNING PHASE - If evacuating, leave with a destination in mind and allow extra time to get there. Take your emergency supplies kit. Gas up your vehicle ahead of time.",
    "WATCH/WARNING PHASE - Let others know where you are going prior to departure.  Secure loose items and pets in the car, and avoid distracted driving.",
    "WATCH/WARNING PHASE - If evacuating, follow designated evacuation routes. Seek traffic information on roadway signs, the radio, and from official sources.",
    "IMMINENT/ONGOING PHASE - Do not enter evacuated areas until officials have given the all clear to return.",
    "RECOVERY PHASE - Do not enter evacuated areas until officials have given the all clear to return."
]

OtherPreparednessActions = {

    "check plans": ["Now is the time to check your emergency plan and emergency supplies kit and take necessary actions to protect your family and secure your home or business.",
                    "When making safety and preparedness decisions, do not focus on the exact forecast track since hazards such as flooding rain, damaging wind gusts, storm surge, and tornadoes  extend well away from the center of the storm.",
                    "If in a place that is vulnerable to high wind, such as near large trees, a manufactured home, upper floors of a high-rise building, or on a boat, plan to move to safe shelter.",
                    "If you live in a place particularly vulnerable to flooding, such as near the ocean or a large inland lake, in a low-lying or poor drainage area, in a valley, or near an already swollen river, plan to move to safe shelter on higher ground.",
                    "Always heed the advice of local officials and comply with orders that are issued. Do not needlessly jeopardize your life or the lives of others.",
                    "When securing your property, outside preparations should be concluded as soon as possible before conditions deteriorate. The onset of strong gusty winds or flooding can cause certain preparedness activities to become unsafe.",
                    "Be sure to let friends and family members know of your intentions for weathering the storm and your whereabouts. Have someone located away from the threatened area serve as your point of contact. Share vital contact information with others. Keep cell phones handy and charged.",
                    "Check on those who may not be fully aware of the situation or who are unable to make personal preparations.",
                    "If you are a visitor, know the name of the county or parish in which you are located and where it is relative to current watches and warnings. If staying at a hotel, ask the management staff about their onsite disaster plan. Listen for evacuation orders, especially pertaining to area visitors.",
                    "Closely monitor weather.gov, NOAA Weather Radio and local news outlets for official storm information. Listen for possible changes to the forecast.",
                    "There is a threat from tornadoes with this storm. Have multiple ways to receive Tornado Warnings. Be ready to shelter quickly."],
    "complete preparations": ["Now is the time to complete all preparations to protect life and property in accordance with your emergency plan. Ensure you are in a safe location before the onset of strong winds or possible flooding.",
                              "If you are relocating to safe shelter, leave as early as possible. Allow extra time to reach your destination. Many roads and bridges will be closed once strong winds arrive. Check the latest weather forecast before departing and drive with caution.",
                              "If heading to a community shelter, become familiar with the shelter rules before arrival, especially if you have special needs or have pets. Take essential items with you from your Emergency Supplies Kit.",
                              "Failure to adequately shelter may result in serious injury or loss of life. Always heed the advice of local officials and comply with any orders that are issued. Remember, during the storm 9 1 1 Emergency Services may not be able to immediately respond if conditions are unsafe. This should be a big factor in your decision making.",
                              "Keep cell phones well charged. Cell phone chargers for automobiles can be helpful, but be aware of your risk for deadly carbon monoxide poisoning if your car is left idling in a garage or other poorly ventilated area.",
                              "It is important to remain calm, informed, and focused during an emergency. Be patient and helpful with those you encounter.",
                              "If you are a visitor, be sure to know the name of the city or town in which you are staying and the name of the county or parish in which it resides. Listen for these locations in local news updates. Pay attention for instructions from local authorities.",
                              "Storm surge is the leading killer associated with tropical storms and hurricanes! Make sure you are in a safe area away from the surge zone. Even if you are not in a surge-prone area, you could find yourself cutoff by flood waters during and after the storm. Heed evacuation orders issued by the local authorities.",
                              "Rapidly rising flood waters are deadly. If you are in a flood-prone area, consider moving to higher ground. Never drive through a flooded roadway. Remember, turn around don't drown!",
                              "If a Tornado Warning is issued for your area, be ready to shelter quickly, preferably away from windows and in an interior room not prone to flooding. If driving, scan the roadside for quick shelter options.",
                              "If in a place that is vulnerable to high wind, such as near large trees, a manufactured home, upper floors of a high-rise building, or on a boat, consider moving to a safer shelter before the onset of strong winds or flooding.",
                              "Closely monitor weather.gov, NOAA Weather radio or local news outlets for official storm information. Be ready to adapt to possible changes to the forecast. Ensure you have multiple ways to receive weather warnings."],
    "hunker down": ["Now is the time to stay inside and away from windows. Listen for updates and be ready in case you lose electrical power. Keep a battery-powered radio, charged cell phone and flashlight handy.",
                    "During the peak of the storm be ready to move quickly. Keep your shoes on and rain gear handy. Boots and athletic shoes offer the best foot protection if you become unexpectedly exposed to the weather.",
                    "Keep your cell phone charged and in power-saving mode. If you lose power, use it sparingly and mainly for personal emergencies and check-ins.",
                    "Do not venture outside while in the eye of a hurricane as any improvement in weather will only be temporary. Once the eye passes, conditions will become life threatening as winds immediately return to dangerous speeds, so remain safely sheltered from the storm.",
                    "Do not be a thrill seeker or risk your life for senseless photos or videos.",
                    "Quickly move to the safest place within your shelter if it begins to fail, preferably an interior room on the lowest floor as long as flooding is not a concern.",
                    "If you are prone to flooding or in an area under a storm surge watch or warning, be prepared for the possibility of a quick and dramatic rise in water levels.",
                    "If a tornado warning is issued for your area, quickly move to the safest place within your shelter. Protect your head and body.",
                    "If an Extreme Wind Warning is issued for your area, move to the safest place within your shelter. Take the same life-saving actions as if it were a violent tornado."],
    "recovery": ["Remain safely sheltered until conditions improve. When going outside be sure to stay away from downed power lines, hazardous debris and flooded areas.",
                 "If your home or shelter was damaged, be alert to the smell of gas leaks and be cautious around electrical wiring, broken glass, jagged metal and wood, and protruding nails and screws.",
                 "Check to see if everyone in your group is OK. Administer first aid to those who are injured. If possible, call 9 1 1 for any serious injuries. Remember, it may be difficult for emergency responders to arrive quickly. ",
                 "Check in with your emergency points of contact. Let them know your location and status. Keep conversations short and to the point. Do not tie up communications systems.",
                 "Check on your neighbors. If necessary, help them connect with their points of contact.",
                 "Do not attempt to return to evacuated areas until local authorities have inspected roads and bridges and have given the all clear.  Hazards like downed power lines and trees, washed out roads, continued flooding in low lying areas and non-functioning traffic lights make travel difficult.",
                 "Allow extra time for emergency vehicles to reach you as they navigate road hazards.",
                 "Do not attempt to return to evacuated areas until local authorities give the all clear. Allow time for officials to inspect bridges and overpasses and to mark washed-out roads.",
                 "When entering areas that have been heavily damaged, bring along a GPS-enabled device to help with street navigation. Do not drive on roads that have been marked closed.",
                 "Do not go sightseeing within impacted communities. Sightseers interfere with the emergency work of first responders.",
                 "When inspecting damage, use flashlights rather than candles or flamed lanterns. Be aware of sparks that can ignite natural gas or other leaking flammables.",
                 "Do not go up on your roof until the weather conditions are safe. Ladders can be slippery in the rain and unexpected wind gusts can blow you off the roof.",
                 "When clearing out fallen trees, be careful with chainsaws and axes. Always wear protective gear and keep others at a safe distance. Leaning trees and those which have fallen on roofs or power lines can be especially dangerous. If you are not in good health or unsure about what you are doing, have someone with tree cutting experience do the job. Never cut trees without a partner.",
                 "If using a generator, avoid carbon monoxide poisoning by following instructions provided by the manufacturer. Operate your generator in a well-ventilated space outside of your living area and away from open doors and windows.",
                 "Problems with sewer backups can further contaminate standing flood waters. Keep children away from flood waters. Also, listen for boil water alerts as tap water may have become non-potable.",
                 "Be alert for any lingering wind gusts which could take down weakened trees and/or power lines, collapse damaged structures, or cause flying debris.",
                 "Be alert for potential flooding from rising rivers and streams which may have yet to crest. Remain informed of the latest river forecasts and heed any flood watches and warnings.",
                 "Be alert for flooded roads which could be compromised or littered with debris. Avoid travel until water levels subside and roads have been cleared. Do not drive through places where flood waters cover the road. Turn around, don't drown!",
                 "Have multiple ways to receive Tornado Warnings if issued. Consider nearby shelter options as you move about. Be ready to shelter quickly."],
}

AdditionalSources = ["- For information on appropriate preparations see ready.gov",
                     "- For information on creating an emergency plan see getagameplan.org",
                     "- For additional disaster preparedness information see redcross.org"]

