#Sample Code to show how the TOTAL_COST may be calculated from the accident train data
#This is useful if you want to project injuries and fatalities separately and combine the
#submodels to calculate the total cost

setwd('c:/DAWG/VicRoads/Final_Data')
acc <- read.csv('accidents_train.csv')

VehicleCosts <- function(CAR,COMMERCIAL,HEAVY,BUS,MOTORCYCLE,BICYCLE,TRAIN,UNKNOWN){
  #average unit cost of repairs
  CAR.cost = 15  
  COMMERCIAL.cost = 20
  HEAVY.cost = 100
  BUS.cost = 100
  MOTORCYCLE.cost = 8
  BICYCLE.cost = 1
  TRAIN.cost = 100
  UNKNOWN.cost = 0
  
  return (CAR * CAR.cost 
          + COMMERCIAL * COMMERCIAL.cost 
          + HEAVY * HEAVY.cost
          + BUS * BUS.cost
          + MOTORCYCLE * MOTORCYCLE.cost
          + BICYCLE * BICYCLE.cost
          + TRAIN * TRAIN.cost
          + UNKNOWN * UNKNOWN.cost
          )
}

PersonCosts <- function(DPNI,DPMI,DPSI,DPF,RPNI,RPMI,RPSI,RPF,PCNI,PCMI,PCSI,PCF,UNI,UMI,USI,UF){
  DRV_PASS_NOT_INJ.cost      =  1
  DRV_PASS_MINOR_INJ.cost     =  8
  DRV_PASS_SERIOUS_INJ.cost   =  100
  DRV_PASS_FATAL.cost         =  200
  RIDER_PILL_NOT_INJ.cost     =  1
  RIDER_PILL_MINOR_INJ.cost   =  15
  RIDER_PILL_SERIOUS_INJ.cost =  100
  RIDER_PILL_FATAL.cost       =  200
  PEDES_CYC_NOT_INJ.cost      =  1
  PEDES_CYC_MINOR_INJ.cost    =  8
  PEDES_CYC_SERIOUS_INJ.cost  =  100
  PEDES_CYC_FATAL.cost        =  200
  UNK_NOT_INJ.cost            =  1
  UNK_MINOR_INJ.cost          =  8
  UNK_SERIOUS_INJ.cost        = 100
  UNK_FATAL.cost              = 200
  
  return( DRV_PASS_NOT_INJ.cost       * DPNI
         +DRV_PASS_MINOR_INJ.cost     * DPMI
         +DRV_PASS_SERIOUS_INJ.cost   * DPSI
         +DRV_PASS_FATAL.cost         * DPF
         +RIDER_PILL_NOT_INJ.cost     * RPNI
         +RIDER_PILL_MINOR_INJ.cost   * RPMI
         +RIDER_PILL_SERIOUS_INJ.cost * RPSI
         +RIDER_PILL_FATAL.cost       * RPF
         +PEDES_CYC_NOT_INJ.cost      * PCNI
         +PEDES_CYC_MINOR_INJ.cost    * PCMI
         +PEDES_CYC_SERIOUS_INJ.cost  * PCSI
         +PEDES_CYC_FATAL.cost        * PCF
         +UNK_NOT_INJ.cost            * UNI
         +UNK_MINOR_INJ.cost          * UMI
         +UNK_SERIOUS_INJ.cost        * USI
         +UNK_FATAL.cost              * UF
  )
}


acc$TEST_VEHICLE_COST <- VehicleCosts(acc$CAR
                                      ,acc$COMMERCIAL
                                      ,acc$HEAVY
                                      ,acc$BUS
                                      ,acc$MOTORCYCLE
                                      ,acc$BICYCLE
                                      ,acc$TRAIN
                                      ,acc$UNKNOWN)

sum(acc$TEST_VEHICLE_COST)
sum(acc$VEHICLE_COST)  

acc$TEST_HUMAN_COST <- PersonCosts(acc$DRV_PASS_NOT_INJ       
                                   ,acc$DRV_PASS_MINOR_INJ
                                   ,acc$DRV_PASS_SERIOUS_INJ
                                   ,acc$DRV_PASS_FATAL      
                                   ,acc$RIDER_PILL_NOT_INJ  
                                   ,acc$RIDER_PILL_MINOR_INJ
                                   ,acc$RIDER_PILL_SERIOUS_INJ
                                   ,acc$RIDER_PILL_FATAL      
                                   ,acc$PEDES_CYC_NOT_INJ     
                                   ,acc$PEDES_CYC_MINOR_INJ   
                                   ,acc$PEDES_CYC_SERIOUS_INJ 
                                   ,acc$PEDES_CYC_FATAL       
                                   ,acc$UNK_NOT_INJ           
                                   ,acc$UNK_MINOR_INJ         
                                   ,acc$UNK_SERIOUS_INJ       
                                   ,acc$UNK_FATAL)             
sum(acc$TEST_HUMAN_COST)
sum(acc$HUMAN_COST)

