package fr.unice.modalis.cosmic.simulator.smartbuilding

import fr.unice.modalis.cosmic.deposit.core.SmartCampusType
import fr.unice.modalis.cosmic.deposit.dsl.DEPOSIT
import fr.unice.modalis.cosmic.simulator.ExperimentalValues

/**
  * Created by Cyril Cecchinel - I3S Laboratory on 22/07/2016.
  */
object TemplatePolicy {
  // Util object. A renamer TemplatePolicy -> OfficeBuilder
  def apply(i:Int) = OfficeBuilder(i.toString)
}

object ComprehensivePolicy extends DEPOSIT{
  this hasForName "Implem_ALERT_AC_BULDING"
  this handles classOf[SmartCampusType]
  this targets ExperimentalValues.INFRA_XML
  val door1 = declare anEventSensor() named "DOOR_1"
  val window1 = declare anEventSensor() named "WINDOW_1"
  val ac1 = declare aPeriodicSensor() named "AC_1" withPeriod 120

  val process1 = define aProcess TemplatePolicy(1)

  val collector1 = declare aCollector() named "SmartCampus"

  flows {
    door1() -> process1("DOOR_1")
    window1() -> process1("WINDOW_1")
    ac1() -> process1("AC_1")
    process1("RESULT") -> collector1()
  }
  val door2 = declare anEventSensor() named "DOOR_2"
  val window2 = declare anEventSensor() named "WINDOW_2"
  val ac2 = declare aPeriodicSensor() named "AC_2" withPeriod 120

  val process2 = define aProcess TemplatePolicy(2)

  val collector2 = declare aCollector() named "SmartCampus"

  flows {
    door2() -> process2("DOOR_2")
    window2() -> process2("WINDOW_2")
    ac2() -> process2("AC_2")
    process2("RESULT") -> collector2()
  }
  val door3 = declare anEventSensor() named "DOOR_3"
  val window3 = declare anEventSensor() named "WINDOW_3"
  val ac3 = declare aPeriodicSensor() named "AC_3" withPeriod 120

  val process3 = define aProcess TemplatePolicy(3)

  val collector3 = declare aCollector() named "SmartCampus"

  flows {
    door3() -> process3("DOOR_3")
    window3() -> process3("WINDOW_3")
    ac3() -> process3("AC_3")
    process3("RESULT") -> collector3()
  }
  val door4 = declare anEventSensor() named "DOOR_4"
  val window4 = declare anEventSensor() named "WINDOW_4"
  val ac4 = declare aPeriodicSensor() named "AC_4" withPeriod 120

  val process4 = define aProcess TemplatePolicy(4)

  val collector4 = declare aCollector() named "SmartCampus"

  flows {
    door4() -> process4("DOOR_4")
    window4() -> process4("WINDOW_4")
    ac4() -> process4("AC_4")
    process4("RESULT") -> collector4()
  }
  val door5 = declare anEventSensor() named "DOOR_5"
  val window5 = declare anEventSensor() named "WINDOW_5"
  val ac5 = declare aPeriodicSensor() named "AC_5" withPeriod 120

  val process5 = define aProcess TemplatePolicy(5)

  val collector5 = declare aCollector() named "SmartCampus"

  flows {
    door5() -> process5("DOOR_5")
    window5() -> process5("WINDOW_5")
    ac5() -> process5("AC_5")
    process5("RESULT") -> collector5()
  }
  val door6 = declare anEventSensor() named "DOOR_6"
  val window6 = declare anEventSensor() named "WINDOW_6"
  val ac6 = declare aPeriodicSensor() named "AC_6" withPeriod 120

  val process6 = define aProcess TemplatePolicy(6)

  val collector6 = declare aCollector() named "SmartCampus"

  flows {
    door6() -> process6("DOOR_6")
    window6() -> process6("WINDOW_6")
    ac6() -> process6("AC_6")
    process6("RESULT") -> collector6()
  }
  val door7 = declare anEventSensor() named "DOOR_7"
  val window7 = declare anEventSensor() named "WINDOW_7"
  val ac7 = declare aPeriodicSensor() named "AC_7" withPeriod 120

  val process7 = define aProcess TemplatePolicy(7)

  val collector7 = declare aCollector() named "SmartCampus"

  flows {
    door7() -> process7("DOOR_7")
    window7() -> process7("WINDOW_7")
    ac7() -> process7("AC_7")
    process7("RESULT") -> collector7()
  }
  val door8 = declare anEventSensor() named "DOOR_8"
  val window8 = declare anEventSensor() named "WINDOW_8"
  val ac8 = declare aPeriodicSensor() named "AC_8" withPeriod 120

  val process8 = define aProcess TemplatePolicy(8)

  val collector8 = declare aCollector() named "SmartCampus"

  flows {
    door8() -> process8("DOOR_8")
    window8() -> process8("WINDOW_8")
    ac8() -> process8("AC_8")
    process8("RESULT") -> collector8()
  }
  val door9 = declare anEventSensor() named "DOOR_9"
  val window9 = declare anEventSensor() named "WINDOW_9"
  val ac9 = declare aPeriodicSensor() named "AC_9" withPeriod 120

  val process9 = define aProcess TemplatePolicy(9)

  val collector9 = declare aCollector() named "SmartCampus"

  flows {
    door9() -> process9("DOOR_9")
    window9() -> process9("WINDOW_9")
    ac9() -> process9("AC_9")
    process9("RESULT") -> collector9()
  }
  val door10 = declare anEventSensor() named "DOOR_10"
  val window10 = declare anEventSensor() named "WINDOW_10"
  val ac10 = declare aPeriodicSensor() named "AC_10" withPeriod 120

  val process10 = define aProcess TemplatePolicy(10)

  val collector10 = declare aCollector() named "SmartCampus"

  flows {
    door10() -> process10("DOOR_10")
    window10() -> process10("WINDOW_10")
    ac10() -> process10("AC_10")
    process10("RESULT") -> collector10()
  }
  val door11 = declare anEventSensor() named "DOOR_11"
  val window11 = declare anEventSensor() named "WINDOW_11"
  val ac11 = declare aPeriodicSensor() named "AC_11" withPeriod 120

  val process11 = define aProcess TemplatePolicy(11)

  val collector11 = declare aCollector() named "SmartCampus"

  flows {
    door11() -> process11("DOOR_11")
    window11() -> process11("WINDOW_11")
    ac11() -> process11("AC_11")
    process11("RESULT") -> collector11()
  }
  val door12 = declare anEventSensor() named "DOOR_12"
  val window12 = declare anEventSensor() named "WINDOW_12"
  val ac12 = declare aPeriodicSensor() named "AC_12" withPeriod 120

  val process12 = define aProcess TemplatePolicy(12)

  val collector12 = declare aCollector() named "SmartCampus"

  flows {
    door12() -> process12("DOOR_12")
    window12() -> process12("WINDOW_12")
    ac12() -> process12("AC_12")
    process12("RESULT") -> collector12()
  }
  val door13 = declare anEventSensor() named "DOOR_13"
  val window13 = declare anEventSensor() named "WINDOW_13"
  val ac13 = declare aPeriodicSensor() named "AC_13" withPeriod 120

  val process13 = define aProcess TemplatePolicy(13)

  val collector13 = declare aCollector() named "SmartCampus"

  flows {
    door13() -> process13("DOOR_13")
    window13() -> process13("WINDOW_13")
    ac13() -> process13("AC_13")
    process13("RESULT") -> collector13()
  }
  val door14 = declare anEventSensor() named "DOOR_14"
  val window14 = declare anEventSensor() named "WINDOW_14"
  val ac14 = declare aPeriodicSensor() named "AC_14" withPeriod 120

  val process14 = define aProcess TemplatePolicy(14)

  val collector14 = declare aCollector() named "SmartCampus"

  flows {
    door14() -> process14("DOOR_14")
    window14() -> process14("WINDOW_14")
    ac14() -> process14("AC_14")
    process14("RESULT") -> collector14()
  }
  val door15 = declare anEventSensor() named "DOOR_15"
  val window15 = declare anEventSensor() named "WINDOW_15"
  val ac15 = declare aPeriodicSensor() named "AC_15" withPeriod 120

  val process15 = define aProcess TemplatePolicy(15)

  val collector15 = declare aCollector() named "SmartCampus"

  flows {
    door15() -> process15("DOOR_15")
    window15() -> process15("WINDOW_15")
    ac15() -> process15("AC_15")
    process15("RESULT") -> collector15()
  }
  val door16 = declare anEventSensor() named "DOOR_16"
  val window16 = declare anEventSensor() named "WINDOW_16"
  val ac16 = declare aPeriodicSensor() named "AC_16" withPeriod 120

  val process16 = define aProcess TemplatePolicy(16)

  val collector16 = declare aCollector() named "SmartCampus"

  flows {
    door16() -> process16("DOOR_16")
    window16() -> process16("WINDOW_16")
    ac16() -> process16("AC_16")
    process16("RESULT") -> collector16()
  }
  val door17 = declare anEventSensor() named "DOOR_17"
  val window17 = declare anEventSensor() named "WINDOW_17"
  val ac17 = declare aPeriodicSensor() named "AC_17" withPeriod 120

  val process17 = define aProcess TemplatePolicy(17)

  val collector17 = declare aCollector() named "SmartCampus"

  flows {
    door17() -> process17("DOOR_17")
    window17() -> process17("WINDOW_17")
    ac17() -> process17("AC_17")
    process17("RESULT") -> collector17()
  }
  val door18 = declare anEventSensor() named "DOOR_18"
  val window18 = declare anEventSensor() named "WINDOW_18"
  val ac18 = declare aPeriodicSensor() named "AC_18" withPeriod 120

  val process18 = define aProcess TemplatePolicy(18)

  val collector18 = declare aCollector() named "SmartCampus"

  flows {
    door18() -> process18("DOOR_18")
    window18() -> process18("WINDOW_18")
    ac18() -> process18("AC_18")
    process18("RESULT") -> collector18()
  }
  val door19 = declare anEventSensor() named "DOOR_19"
  val window19 = declare anEventSensor() named "WINDOW_19"
  val ac19 = declare aPeriodicSensor() named "AC_19" withPeriod 120

  val process19 = define aProcess TemplatePolicy(19)

  val collector19 = declare aCollector() named "SmartCampus"

  flows {
    door19() -> process19("DOOR_19")
    window19() -> process19("WINDOW_19")
    ac19() -> process19("AC_19")
    process19("RESULT") -> collector19()
  }
  val door20 = declare anEventSensor() named "DOOR_20"
  val window20 = declare anEventSensor() named "WINDOW_20"
  val ac20 = declare aPeriodicSensor() named "AC_20" withPeriod 120

  val process20 = define aProcess TemplatePolicy(20)

  val collector20 = declare aCollector() named "SmartCampus"

  flows {
    door20() -> process20("DOOR_20")
    window20() -> process20("WINDOW_20")
    ac20() -> process20("AC_20")
    process20("RESULT") -> collector20()
  }
  val door21 = declare anEventSensor() named "DOOR_21"
  val window21 = declare anEventSensor() named "WINDOW_21"
  val ac21 = declare aPeriodicSensor() named "AC_21" withPeriod 120

  val process21 = define aProcess TemplatePolicy(21)

  val collector21 = declare aCollector() named "SmartCampus"

  flows {
    door21() -> process21("DOOR_21")
    window21() -> process21("WINDOW_21")
    ac21() -> process21("AC_21")
    process21("RESULT") -> collector21()
  }
  val door22 = declare anEventSensor() named "DOOR_22"
  val window22 = declare anEventSensor() named "WINDOW_22"
  val ac22 = declare aPeriodicSensor() named "AC_22" withPeriod 120

  val process22 = define aProcess TemplatePolicy(22)

  val collector22 = declare aCollector() named "SmartCampus"

  flows {
    door22() -> process22("DOOR_22")
    window22() -> process22("WINDOW_22")
    ac22() -> process22("AC_22")
    process22("RESULT") -> collector22()
  }
  val door23 = declare anEventSensor() named "DOOR_23"
  val window23 = declare anEventSensor() named "WINDOW_23"
  val ac23 = declare aPeriodicSensor() named "AC_23" withPeriod 120

  val process23 = define aProcess TemplatePolicy(23)

  val collector23 = declare aCollector() named "SmartCampus"

  flows {
    door23() -> process23("DOOR_23")
    window23() -> process23("WINDOW_23")
    ac23() -> process23("AC_23")
    process23("RESULT") -> collector23()
  }
  val door24 = declare anEventSensor() named "DOOR_24"
  val window24 = declare anEventSensor() named "WINDOW_24"
  val ac24 = declare aPeriodicSensor() named "AC_24" withPeriod 120

  val process24 = define aProcess TemplatePolicy(24)

  val collector24 = declare aCollector() named "SmartCampus"

  flows {
    door24() -> process24("DOOR_24")
    window24() -> process24("WINDOW_24")
    ac24() -> process24("AC_24")
    process24("RESULT") -> collector24()
  }
  val door25 = declare anEventSensor() named "DOOR_25"
  val window25 = declare anEventSensor() named "WINDOW_25"
  val ac25 = declare aPeriodicSensor() named "AC_25" withPeriod 120

  val process25 = define aProcess TemplatePolicy(25)

  val collector25 = declare aCollector() named "SmartCampus"

  flows {
    door25() -> process25("DOOR_25")
    window25() -> process25("WINDOW_25")
    ac25() -> process25("AC_25")
    process25("RESULT") -> collector25()
  }
  val door26 = declare anEventSensor() named "DOOR_26"
  val window26 = declare anEventSensor() named "WINDOW_26"
  val ac26 = declare aPeriodicSensor() named "AC_26" withPeriod 120

  val process26 = define aProcess TemplatePolicy(26)

  val collector26 = declare aCollector() named "SmartCampus"

  flows {
    door26() -> process26("DOOR_26")
    window26() -> process26("WINDOW_26")
    ac26() -> process26("AC_26")
    process26("RESULT") -> collector26()
  }
  val door27 = declare anEventSensor() named "DOOR_27"
  val window27 = declare anEventSensor() named "WINDOW_27"
  val ac27 = declare aPeriodicSensor() named "AC_27" withPeriod 120

  val process27 = define aProcess TemplatePolicy(27)

  val collector27 = declare aCollector() named "SmartCampus"

  flows {
    door27() -> process27("DOOR_27")
    window27() -> process27("WINDOW_27")
    ac27() -> process27("AC_27")
    process27("RESULT") -> collector27()
  }
  val door28 = declare anEventSensor() named "DOOR_28"
  val window28 = declare anEventSensor() named "WINDOW_28"
  val ac28 = declare aPeriodicSensor() named "AC_28" withPeriod 120

  val process28 = define aProcess TemplatePolicy(28)

  val collector28 = declare aCollector() named "SmartCampus"

  flows {
    door28() -> process28("DOOR_28")
    window28() -> process28("WINDOW_28")
    ac28() -> process28("AC_28")
    process28("RESULT") -> collector28()
  }
  val door29 = declare anEventSensor() named "DOOR_29"
  val window29 = declare anEventSensor() named "WINDOW_29"
  val ac29 = declare aPeriodicSensor() named "AC_29" withPeriod 120

  val process29 = define aProcess TemplatePolicy(29)

  val collector29 = declare aCollector() named "SmartCampus"

  flows {
    door29() -> process29("DOOR_29")
    window29() -> process29("WINDOW_29")
    ac29() -> process29("AC_29")
    process29("RESULT") -> collector29()
  }
  val door30 = declare anEventSensor() named "DOOR_30"
  val window30 = declare anEventSensor() named "WINDOW_30"
  val ac30 = declare aPeriodicSensor() named "AC_30" withPeriod 120

  val process30 = define aProcess TemplatePolicy(30)

  val collector30 = declare aCollector() named "SmartCampus"

  flows {
    door30() -> process30("DOOR_30")
    window30() -> process30("WINDOW_30")
    ac30() -> process30("AC_30")
    process30("RESULT") -> collector30()
  }
  val door31 = declare anEventSensor() named "DOOR_31"
  val window31 = declare anEventSensor() named "WINDOW_31"
  val ac31 = declare aPeriodicSensor() named "AC_31" withPeriod 120

  val process31 = define aProcess TemplatePolicy(31)

  val collector31 = declare aCollector() named "SmartCampus"

  flows {
    door31() -> process31("DOOR_31")
    window31() -> process31("WINDOW_31")
    ac31() -> process31("AC_31")
    process31("RESULT") -> collector31()
  }
  val door32 = declare anEventSensor() named "DOOR_32"
  val window32 = declare anEventSensor() named "WINDOW_32"
  val ac32 = declare aPeriodicSensor() named "AC_32" withPeriod 120

  val process32 = define aProcess TemplatePolicy(32)

  val collector32 = declare aCollector() named "SmartCampus"

  flows {
    door32() -> process32("DOOR_32")
    window32() -> process32("WINDOW_32")
    ac32() -> process32("AC_32")
    process32("RESULT") -> collector32()
  }
  val door33 = declare anEventSensor() named "DOOR_33"
  val window33 = declare anEventSensor() named "WINDOW_33"
  val ac33 = declare aPeriodicSensor() named "AC_33" withPeriod 120

  val process33 = define aProcess TemplatePolicy(33)

  val collector33 = declare aCollector() named "SmartCampus"

  flows {
    door33() -> process33("DOOR_33")
    window33() -> process33("WINDOW_33")
    ac33() -> process33("AC_33")
    process33("RESULT") -> collector33()
  }
  val door34 = declare anEventSensor() named "DOOR_34"
  val window34 = declare anEventSensor() named "WINDOW_34"
  val ac34 = declare aPeriodicSensor() named "AC_34" withPeriod 120

  val process34 = define aProcess TemplatePolicy(34)

  val collector34 = declare aCollector() named "SmartCampus"

  flows {
    door34() -> process34("DOOR_34")
    window34() -> process34("WINDOW_34")
    ac34() -> process34("AC_34")
    process34("RESULT") -> collector34()
  }
  val door35 = declare anEventSensor() named "DOOR_35"
  val window35 = declare anEventSensor() named "WINDOW_35"
  val ac35 = declare aPeriodicSensor() named "AC_35" withPeriod 120

  val process35 = define aProcess TemplatePolicy(35)

  val collector35 = declare aCollector() named "SmartCampus"

  flows {
    door35() -> process35("DOOR_35")
    window35() -> process35("WINDOW_35")
    ac35() -> process35("AC_35")
    process35("RESULT") -> collector35()
  }
  val door36 = declare anEventSensor() named "DOOR_36"
  val window36 = declare anEventSensor() named "WINDOW_36"
  val ac36 = declare aPeriodicSensor() named "AC_36" withPeriod 120

  val process36 = define aProcess TemplatePolicy(36)

  val collector36 = declare aCollector() named "SmartCampus"

  flows {
    door36() -> process36("DOOR_36")
    window36() -> process36("WINDOW_36")
    ac36() -> process36("AC_36")
    process36("RESULT") -> collector36()
  }
  val door37 = declare anEventSensor() named "DOOR_37"
  val window37 = declare anEventSensor() named "WINDOW_37"
  val ac37 = declare aPeriodicSensor() named "AC_37" withPeriod 120

  val process37 = define aProcess TemplatePolicy(37)

  val collector37 = declare aCollector() named "SmartCampus"

  flows {
    door37() -> process37("DOOR_37")
    window37() -> process37("WINDOW_37")
    ac37() -> process37("AC_37")
    process37("RESULT") -> collector37()
  }
  val door38 = declare anEventSensor() named "DOOR_38"
  val window38 = declare anEventSensor() named "WINDOW_38"
  val ac38 = declare aPeriodicSensor() named "AC_38" withPeriod 120

  val process38 = define aProcess TemplatePolicy(38)

  val collector38 = declare aCollector() named "SmartCampus"

  flows {
    door38() -> process38("DOOR_38")
    window38() -> process38("WINDOW_38")
    ac38() -> process38("AC_38")
    process38("RESULT") -> collector38()
  }
  val door39 = declare anEventSensor() named "DOOR_39"
  val window39 = declare anEventSensor() named "WINDOW_39"
  val ac39 = declare aPeriodicSensor() named "AC_39" withPeriod 120

  val process39 = define aProcess TemplatePolicy(39)

  val collector39 = declare aCollector() named "SmartCampus"

  flows {
    door39() -> process39("DOOR_39")
    window39() -> process39("WINDOW_39")
    ac39() -> process39("AC_39")
    process39("RESULT") -> collector39()
  }
  val door40 = declare anEventSensor() named "DOOR_40"
  val window40 = declare anEventSensor() named "WINDOW_40"
  val ac40 = declare aPeriodicSensor() named "AC_40" withPeriod 120

  val process40 = define aProcess TemplatePolicy(40)

  val collector40 = declare aCollector() named "SmartCampus"

  flows {
    door40() -> process40("DOOR_40")
    window40() -> process40("WINDOW_40")
    ac40() -> process40("AC_40")
    process40("RESULT") -> collector40()
  }
  val door41 = declare anEventSensor() named "DOOR_41"
  val window41 = declare anEventSensor() named "WINDOW_41"
  val ac41 = declare aPeriodicSensor() named "AC_41" withPeriod 120

  val process41 = define aProcess TemplatePolicy(41)

  val collector41 = declare aCollector() named "SmartCampus"

  flows {
    door41() -> process41("DOOR_41")
    window41() -> process41("WINDOW_41")
    ac41() -> process41("AC_41")
    process41("RESULT") -> collector41()
  }
  val door42 = declare anEventSensor() named "DOOR_42"
  val window42 = declare anEventSensor() named "WINDOW_42"
  val ac42 = declare aPeriodicSensor() named "AC_42" withPeriod 120

  val process42 = define aProcess TemplatePolicy(42)

  val collector42 = declare aCollector() named "SmartCampus"

  flows {
    door42() -> process42("DOOR_42")
    window42() -> process42("WINDOW_42")
    ac42() -> process42("AC_42")
    process42("RESULT") -> collector42()
  }
  val door43 = declare anEventSensor() named "DOOR_43"
  val window43 = declare anEventSensor() named "WINDOW_43"
  val ac43 = declare aPeriodicSensor() named "AC_43" withPeriod 120

  val process43 = define aProcess TemplatePolicy(43)

  val collector43 = declare aCollector() named "SmartCampus"

  flows {
    door43() -> process43("DOOR_43")
    window43() -> process43("WINDOW_43")
    ac43() -> process43("AC_43")
    process43("RESULT") -> collector43()
  }
  val door44 = declare anEventSensor() named "DOOR_44"
  val window44 = declare anEventSensor() named "WINDOW_44"
  val ac44 = declare aPeriodicSensor() named "AC_44" withPeriod 120

  val process44 = define aProcess TemplatePolicy(44)

  val collector44 = declare aCollector() named "SmartCampus"

  flows {
    door44() -> process44("DOOR_44")
    window44() -> process44("WINDOW_44")
    ac44() -> process44("AC_44")
    process44("RESULT") -> collector44()
  }
  val door45 = declare anEventSensor() named "DOOR_45"
  val window45 = declare anEventSensor() named "WINDOW_45"
  val ac45 = declare aPeriodicSensor() named "AC_45" withPeriod 120

  val process45 = define aProcess TemplatePolicy(45)

  val collector45 = declare aCollector() named "SmartCampus"

  flows {
    door45() -> process45("DOOR_45")
    window45() -> process45("WINDOW_45")
    ac45() -> process45("AC_45")
    process45("RESULT") -> collector45()
  }
  val door46 = declare anEventSensor() named "DOOR_46"
  val window46 = declare anEventSensor() named "WINDOW_46"
  val ac46 = declare aPeriodicSensor() named "AC_46" withPeriod 120

  val process46 = define aProcess TemplatePolicy(46)

  val collector46 = declare aCollector() named "SmartCampus"

  flows {
    door46() -> process46("DOOR_46")
    window46() -> process46("WINDOW_46")
    ac46() -> process46("AC_46")
    process46("RESULT") -> collector46()
  }
  val door47 = declare anEventSensor() named "DOOR_47"
  val window47 = declare anEventSensor() named "WINDOW_47"
  val ac47 = declare aPeriodicSensor() named "AC_47" withPeriod 120

  val process47 = define aProcess TemplatePolicy(47)

  val collector47 = declare aCollector() named "SmartCampus"

  flows {
    door47() -> process47("DOOR_47")
    window47() -> process47("WINDOW_47")
    ac47() -> process47("AC_47")
    process47("RESULT") -> collector47()
  }
  val door48 = declare anEventSensor() named "DOOR_48"
  val window48 = declare anEventSensor() named "WINDOW_48"
  val ac48 = declare aPeriodicSensor() named "AC_48" withPeriod 120

  val process48 = define aProcess TemplatePolicy(48)

  val collector48 = declare aCollector() named "SmartCampus"

  flows {
    door48() -> process48("DOOR_48")
    window48() -> process48("WINDOW_48")
    ac48() -> process48("AC_48")
    process48("RESULT") -> collector48()
  }
  val door49 = declare anEventSensor() named "DOOR_49"
  val window49 = declare anEventSensor() named "WINDOW_49"
  val ac49 = declare aPeriodicSensor() named "AC_49" withPeriod 120

  val process49 = define aProcess TemplatePolicy(49)

  val collector49 = declare aCollector() named "SmartCampus"

  flows {
    door49() -> process49("DOOR_49")
    window49() -> process49("WINDOW_49")
    ac49() -> process49("AC_49")
    process49("RESULT") -> collector49()
  }
  val door50 = declare anEventSensor() named "DOOR_50"
  val window50 = declare anEventSensor() named "WINDOW_50"
  val ac50 = declare aPeriodicSensor() named "AC_50" withPeriod 120

  val process50 = define aProcess TemplatePolicy(50)

  val collector50 = declare aCollector() named "SmartCampus"

  flows {
    door50() -> process50("DOOR_50")
    window50() -> process50("WINDOW_50")
    ac50() -> process50("AC_50")
    process50("RESULT") -> collector50()
  }

}