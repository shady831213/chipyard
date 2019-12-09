#ifndef __EMULATOR_H
#define __EMULATOR_H

#include <fesvr/tsi.h>
class ftsi_t : public tsi_t
{
 public:
  ftsi_t(int argc, char** argv);
  virtual ~ftsi_t();

 protected:
  virtual void load_program() override;
};

//class hexwriter_t: public htif_hexwriter_t{
//public:
//  hexwriter_t(size_t b, size_t w, size_t d);
//protected:
//  friend std::ostream& operator<< (std::ostream&, const hexwriter_t&);
//};

void _load_program(const std::vector<std::string>& targs, reg_t* entry, addr_t* tohost_addr, addr_t* fromhost_addr, addr_t* sig_addr, addr_t* sig_len);

#endif