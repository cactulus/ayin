struct Find_Result {
    int windows_sdk_version;   // Zero if no Windows SDK found.

    wchar_t *windows_sdk_root = 0;
    wchar_t *windows_sdk_um_library_path = 0;
    wchar_t *windows_sdk_ucrt_library_path = 0;

    wchar_t *vs_exe_path = 0;
    wchar_t *vs_library_path = 0;
};

Find_Result find_visual_studio_and_windows_sdk();
