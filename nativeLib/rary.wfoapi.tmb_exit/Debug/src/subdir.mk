################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_UPPER_SRCS += \
../src/sockhelp.C \
../src/tmb_exit.C 

OBJS += \
./src/sockhelp.o \
./src/tmb_exit.o 

C_UPPER_DEPS += \
./src/sockhelp.d \
./src/tmb_exit.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.C
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	../../build.native/tools/compile.sh -I../../rary.wfoapi.common/include -I../../rary.wfoapi.awips-common/include -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


