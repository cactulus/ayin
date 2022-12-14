workspace "ayin"
	configurations { "Debug", "Release", "Dist" }
	filter { "platforms:*32" } architecture "x86"
  	filter { "platforms:*64" } architecture "x64"

project "ayin"
	kind "ConsoleApp"
	language "C++"
	targetdir "bin/%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"
	objdir "build/%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"
	cppdialect "C++17"

	files { "src/*.h", "src/*.cpp" }

	filter "system:windows"
		architecture "x64"
		staticruntime "On"
		systemversion "10.0.19041.0"
  		includedirs { "llvm/include" }
  		linkoptions { "llvm/lib/*.lib" }

	filter { "system:linux" }
  		buildoptions { "`llvm-config --cxxflags`" }
  		linkoptions { "`llvm-config --ldflags --libs core`" }
		buildoptions { "-Wno-writable-strings -Wno-switch" }

	filter { "system:macosx" }
  		buildoptions { "`llvm-config --cxxflags`" }
  		linkoptions { "`llvm-config --ldflags --libs all`" }
		buildoptions { "-Wno-writable-strings -Wno-switch" }

	filter "configurations:Debug"
		defines { "AYIN_DEBUG" }
		symbols "On"

	filter "configurations:Release"
		defines { "AYIN_RELEASE" }
		optimize "On"

	filter "configurations:Dist"
		defines { "AYIN_DIST" }
		optimize "On"
