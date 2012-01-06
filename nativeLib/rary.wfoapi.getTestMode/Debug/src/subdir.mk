################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_UPPER_SRCS += \
../src/getTestMode.C \
../src/sockhelp.C 

OBJS += \
./src/getTestMode.o \
./src/sockhelp.o 

C_UPPER_DEPS += \
./src/getTestMode.d \
./src/sockhelp.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.C
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	../../build.native/tools/compile.sh -I../../rary.wfoapi.common/include -I../../rary.wfoapi.awips-common/include -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


