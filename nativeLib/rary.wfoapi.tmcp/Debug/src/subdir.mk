################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_UPPER_SRCS += \
../src/tmbUtil.C \
../src/tmcp.C 

OBJS += \
./src/tmbUtil.o \
./src/tmcp.o 

C_UPPER_DEPS += \
./src/tmbUtil.d \
./src/tmcp.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.C
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	../../build.native/tools/compile.sh -I../../rary.wfoapi.common/include -I../../rary.empty.motif/include -I../include -I../../rary.wfoapi.awips-common/include -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


