################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_UPPER_SRCS += \
../src/WorkstationTestMode.C \
../src/sockhelp.C \
../src/test_WorkstationTestMode.C 

OBJS += \
./src/WorkstationTestMode.o \
./src/sockhelp.o \
./src/test_WorkstationTestMode.o 

C_UPPER_DEPS += \
./src/WorkstationTestMode.d \
./src/sockhelp.d \
./src/test_WorkstationTestMode.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.C
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	../../build.native/tools/compile.sh -I../include -I../../rary.wfoapi.awips-common/include -I../../rary.wfoapi.common/include -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


