; ModuleID = 'Aleph Module'
source_filename = "Aleph Module"

define i64 @add_s64s64(i64 %0, i64 %1) {
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 4
  %4 = alloca i64, align 8
  store i64 %1, i64* %4, align 4
  %5 = load i64, i64* %3, align 4
  ret i64 %5
}

define double @add_f64f64(double %0, double %1) {
  %3 = alloca double, align 8
  store double %0, double* %3, align 8
  %4 = alloca double, align 8
  store double %1, double* %4, align 8
  %5 = load double, double* %3, align 8
  ret double %5
}

define i32 @main() {
  %1 = alloca { i32, i32 }, align 8
  %2 = alloca i64, align 8
  %3 = call i64 @add_s64s64(i64 2, i64 4)
  store i64 %3, i64* %2, align 4
  %4 = alloca double, align 8
  %5 = call double @add_f64f64(double 2.000000e+00, double 4.000000e+00)
  store double %5, double* %4, align 8
  ret double 0.000000e+00
}
