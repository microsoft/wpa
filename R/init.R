# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

.onAttach <- function(libname, pkgname) {
  message <- c("\n Thank you for using v1.4.3 of the {wpa} R package!",
               "\n \n Our analysts have taken every care to ensure that this package runs smoothly and bug-free.",
               "\n \n However, if you do happen to encounter any, please report any issues at",
               "\n https://github.com/microsoft/wpa/issues/",
               "\n \n Happy coding!")
  packageStartupMessage(message)
}
