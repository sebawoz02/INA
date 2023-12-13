//
// Created by sebawoz02 on 13.12.23.
//
#include <pixel.h>

[[nodiscard]] bool Pixel::are_equal(Pixel p) const
{
  return red == p.red && green == p.green && blue == p.blue;
}
