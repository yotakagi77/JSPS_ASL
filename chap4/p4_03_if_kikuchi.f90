module interface_403    
    interface
        function revchar(c) result(rc)
            character(*), intent(in) :: c
            character(len(c)) rc
            integer i
        end function revchar
    end interface
end module interface_403