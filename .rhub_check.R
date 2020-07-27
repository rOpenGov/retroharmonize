
library(rhub)

rhub::check(
  platform="windows-x86_64-devel",
  env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
)

rhub::check(
  platform="windows-x86_64-release",
  env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
)
