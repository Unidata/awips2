################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_UPPER_SRCS += \
../src/Procstruct.C \
../src/consoleUser.C \
../src/getConsoleUser.C 

OBJS += \
./src/Procstruct.o \
./src/consoleUser.o \
./src/getConsoleUser.o 

C_UPPER_DEPS += \
./src/Procstruct.d \
./src/consoleUser.d \
./src/getConsoleUser.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.C
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	../../build.native/tools/compile.sh -I../include -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


