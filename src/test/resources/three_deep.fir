circuit Top :
    module Memory :
        input clk : Clock
        input reset : UInt<1>
        input addr : UInt<16>
        output data : UInt<16>

        mem tag_arr_3 :
            data-type => UInt<16>
            depth => 64
            read-latency => 1
            write-latency => 1
            reader => T_1111
            writer => T_1751
            read-under-write => undefined
        tag_arr_3.T_1111.addr <= addr
        tag_arr_3.T_1111.en <= UInt<1>("h01")
        data <= tag_arr_3.T_1111.data

    module Level1 :
        input clk : Clock
        input reset : UInt<1>
        input addr : UInt<16>
        output data : UInt<16>

        inst level2 of Level2

        level2.addr <= addr
        data <= level2.data

    module Level2 :
        input clk : Clock
        input reset : UInt<1>
        input addr : UInt<16>
        output data : UInt<16>

        inst memory of Memory

        memory.addr <= addr
        data <= memory.data

    module Top :
        input clk : Clock
        input reset : UInt<1>
        input addr : UInt<16>
        output data : UInt<16>

        inst level1 of Level1

        level1.addr <= addr
        data <= level1.data




