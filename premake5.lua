workspace "aleph"
	configurations { "Debug", "Release", "Dist" }
	architecture "x64"

project "aleph"
	kind "ConsoleApp"
	language "C++"
	targetdir "bin/%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"
	objdir "build/%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"

	files { "src/*.h", "src/*.cpp" }

	filter "system:windows"
		cppdialect "C++17"
		staticruntime "On"
		systemversion "10.0.19041.0"

	filter "configurations:Debug"
		defines { "ALEPH_DEBUG" }
		symbols "On"

	filter "configurations:Release"
		defines { "ALEPH_RELEASE" }
		optimize "On"

	filter "configurations:Dist"
		defines { "ALEPH_DIST" }
		optimize "On"
