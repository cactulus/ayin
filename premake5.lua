workspace "aleph"
	configurations { "Debug", "Release", "Dist" }
	filter { "platforms:*32" } architecture "x86"
  	filter { "platforms:*64" } architecture "x64"

project "aleph"
	kind "ConsoleApp"
	language "C++"
	targetdir "bin/%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"
	objdir "build/%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"

	files { "src/*.h", "src/*.cpp" }

	filter "system:windows"
		cppdialect "C++17"
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
  		linkoptions { "`llvm-config --ldflags --libs core`" }
		buildoptions { "-Wno-writable-strings -Wno-switch" }

	filter "configurations:Debug"
		defines { "ALEPH_DEBUG" }
		symbols "On"

	filter "configurations:Release"
		defines { "ALEPH_RELEASE" }
		optimize "On"

	filter "configurations:Dist"
		defines { "ALEPH_DIST" }
		optimize "On"
